defmodule AdventOfCode.Y2016.Day10.Bot do
  @moduledoc """
  Bot worker. Each represents a bot with low/high interaction bots and assignability.
  """
  alias AdventOfCode.Y2016.Day10.{OutputBin, Bot}

  use GenServer, restart: :transient

  def start_link(name) do
    GenServer.start_link(Bot, String.to_integer(name), name: {:global, name})
  end

  def assign(pid, val), do: GenServer.cast(pid, {:assign, val})

  def configure(pid, {lo_module, lo_pid}, {hi_module, hi_pid}) do
    GenServer.cast(
      pid,
      {:configure, {lo_module, lo_pid}, {hi_module, hi_pid}}
    )
  end

  def vals(pid) do
    GenServer.call(pid, :vals)
  end

  @impl true
  def init(name), do: {:ok, %{vals: [], name: name}}

  @impl true
  def handle_cast({:assign, val}, state) do
    state = Map.update(state, :vals, [val], &[val | &1])

    case state.vals do
      [_, _] = vals ->
        {lo, hi} = Enum.min_max(vals)
        send_to(lo, state.lo_module, state.lo_pid)
        send_to(hi, state.hi_module, state.hi_pid)

      _ ->
        nil
    end

    {:noreply, %{state | vals: Enum.sort(state.vals)}}
  end

  @impl true
  def handle_cast({:configure, {lo_module, lo_pid}, {hi_module, hi_pid}}, state) do
    {:noreply,
     Map.merge(state, %{
       lo_module: lo_module,
       hi_module: hi_module,
       lo_pid: lo_pid,
       hi_pid: hi_pid
     })}
  end

  @impl true
  def handle_call(:vals, _from, %{vals: vals, name: name} = state) do
    {:reply, {name, vals}, state}
  end

  defp send_to(val, __MODULE__, pid) do
    Bot.assign(pid, val)
  end

  defp send_to(val, _, pid) do
    Agent.update(OutputBin, fn state -> Map.merge(state, %{pid => val}) end)
  end
end
