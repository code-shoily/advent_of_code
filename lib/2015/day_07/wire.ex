defmodule AdventOfCode.Y2015.Day07.Wire do
  @moduledoc """
  Worker for wires.
  """
  alias __MODULE__

  use GenServer, restart: :temporary

  import Bitwise

  @mapping %{
    assign: &Function.identity/1,
    lshift: &bsl/2,
    rshift: &bsr/2,
    or: &bor/2,
    and: &band/2,
    not: &bnot/1
  }

  ##########################################
  # PUBLIC API
  ##########################################
  def start_link(wire) do
    GenServer.start_link(Wire, wire, name: {:global, wire})
  end

  def add_relation(pid, relation, providers) do
    GenServer.call(pid, {:add_relation, relation, providers})
  end

  def assign_signal(pid, signal) do
    GenServer.cast(pid, {:assign, signal})
  end

  ##########################################
  # IMPLEMENTATION
  ##########################################
  @impl true
  def init(wire) do
    {:ok,
     %{
       wire: wire,
       signal: nil,
       providers: [],
       provides: [],
       relation: nil
     }}
  end

  @impl true
  def handle_call({:add_relation, relation, providers}, _, state) do
    Enum.each(providers, fn provider ->
      GenServer.cast(self(), {:depends_on, provider})
    end)

    {:reply, :done,
     %{
       state
       | relation: relation,
         providers:
           Enum.map(
             providers,
             fn provider -> :global.whereis_name(provider) end
           )
     }}
  end

  @impl true
  def handle_cast({:provides_to, dependant}, %{provides: provides} = state) do
    {:noreply, %{state | provides: [:global.whereis_name(dependant) | provides]}}
  end

  @impl true
  def handle_cast({:depends_on, provider}, %{wire: wire} = state) do
    GenServer.cast(:global.whereis_name(provider), {:provides_to, wire})
    {:noreply, state}
  end

  @impl true
  def handle_cast({:assign, signal}, %{wire: provider, provides: provides} = state) do
    Enum.each(provides, fn pid ->
      GenServer.cast(pid, {:receive_signal, provider, signal})
    end)

    {:noreply, %{state | signal: signal}}
  end

  @impl true
  def handle_cast({:receive_signal, providing_wire, signal}, %{relation: relation} = state) do
    relation = replace_signal(relation, providing_wire, signal)

    if signal = get_signal(relation) do
      GenServer.cast(self(), {:assign, signal})
    end

    {:noreply, %{state | relation: relation}}
  end

  ##########################################
  # PRIVATE FUNCTIONS
  ##########################################
  def get_signal(relation) do
    case relation do
      {op, signal} when is_integer(signal) ->
        apply(@mapping[op], [signal])

      {op, signal_a, signal_b} when is_integer(signal_a) and is_integer(signal_b) ->
        apply(@mapping[op], [signal_a, signal_b])

      _ ->
        nil
    end
  end

  def replace_signal(relation, provider, signal) when is_binary(provider) do
    case relation do
      {op, ^provider} -> {op, signal}
      {op, ^provider, other} -> {op, signal, other}
      {op, other, ^provider} -> {op, other, signal}
      relation -> relation
    end
  end
end
