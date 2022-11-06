defmodule AdventOfCode.Y2015.Day07.ControlPanel do
  @moduledoc """
  ControlPanel for managing Wire Workers.
  """
  alias AdventOfCode.Y2015.Day07.{RootSupervisor, Wire, WireSupervisor}

  @doc """
  Start all the servers. Both the output container and the dynamic supervisor
  """
  def start_servers do
    children = [
      {DynamicSupervisor, strategy: :one_for_one, name: WireSupervisor}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: RootSupervisor)
  end

  @doc """
  Stops the servers
  """
  def stop_servers do
    Supervisor.stop(RootSupervisor)
  end

  @doc """
  Create all bots for all name in names.
  """
  def create_wires({assignments, relations}) do
    assignments
    |> Kernel.++(relations)
    |> Enum.map(fn {k, _} -> k end)
    |> Enum.each(&DynamicSupervisor.start_child(WireSupervisor, {Wire, &1}))
  end

  @doc """
  Builds dependency tree among wires
  """
  def build_dependencies(wires) do
    Enum.map(wires, fn {wire, %{relation: relation, providers: providers}} ->
      Wire.add_relation(
        :global.whereis_name(wire),
        relation,
        providers
      )
    end)
  end

  @doc """
  Assigns all the workers based on given instructions
  """
  def provide_signals(assignments) do
    Enum.each(assignments, fn {wire, %{relation: {:assign, signal}}} ->
      spawn(fn ->
        Wire.assign_signal(
          :global.whereis_name(wire),
          signal
        )
      end)
    end)
  end

  @doc """
  Gets the value of given wire
  """
  def get_signal(wire) do
    maybe_signal =
      wire
      |> :global.whereis_name()
      |> :sys.get_state()
      |> Map.get(:signal)

    if maybe_signal do
      maybe_signal
    else
      get_signal(wire)
    end
  end
end
