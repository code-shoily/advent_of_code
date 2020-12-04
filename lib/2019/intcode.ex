defmodule AdventOfCode.Y2019.IntCode do
  @moduledoc false

  use GenServer

  @type instruction :: non_neg_integer()
  @type padded_instruction() :: binary()
  @type memory :: list(integer())
  @type state :: %{
          data: memory(),
          iptr: non_neg_integer() | :halt,
          next: list(binary()),
          output: list(any()),
          start_val: any()
        }

  # ------------------------- COMMANDS
  @store "00003"
  # Binary operations
  @bins [3, 4, 104]
  @halt "00099"

  @doc """
  Starts an IntCode computer by taking in a list of integers as instructionset.
  """
  @spec start_link(memory()) :: GenServer.on_start()
  def start_link(opts), do: GenServer.start_link(__MODULE__, opts)

  @doc """
  Returns the current state of the server
  """
  @spec get_state(pid()) :: state()
  def get_state(pid), do: GenServer.call(pid, :state)

  @doc """
  Returns the output of the run
  """
  def get_output(pid), do: GenServer.call(pid, :output)

  @doc """
  Resets the computer with new memory feed
  """
  def reset(pid, data), do: GenServer.call(pid, {:reset, data})

  @spec run(pid()) :: state()
  def run(pid) do
    case get_state(pid) do
      %{iptr: :halt} ->
        get_state(pid)

      %{next: next} ->
        GenServer.call(pid, {:cmd, next})
        run(pid)
    end
  end

  @impl true
  def init(data) do
    {:ok,
     %{
       data: data,
       next: tokenize(data),
       iptr: 0,
       output: [],
       start_val: 1
     }}
  end

  @impl true
  def handle_call(:state, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call(:output, _from, %{data: [out | _]} = state), do: {:reply, out, state}
  def handle_call(:output, _from, state), do: {:reply, nil, state}

  @impl true
  def handle_call({:reset, data}, _from, _) do
    {:ok, state} = init(data)
    {:reply, state, state}
  end

  @impl true
  def handle_call({:cmd, [@halt | _]}, _from, state) do
    state = Map.put(state, :iptr, :halt)
    {:reply, state, state}
  end

  @impl true
  def handle_call(
        {:cmd, [@store, addr]},
        _from,
        %{start_val: start_val, data: data} = state
      ) do
    data = List.replace_at(data, addr, start_val)

    state =
      state
      |> Map.put(:data, data)
      |> Map.update(:iptr, 0, fn iptr -> iptr + 2 end)
      |> Map.put(:next, tokenize(Enum.drop(data, 2)))

    {:reply, state, state}
  end

  @impl true
  def handle_call(
        {:cmd, vector},
        _from,
        %{data: data, output: output, iptr: iptr} = state
      ) do
    {data, output} = execute(vector, data, output)
    ptr_incr = iptr + length(vector)

    state =
      state
      |> Map.merge(%{
        data: data,
        iptr: ptr_incr,
        output: output,
        next: tokenize(Enum.drop(data, ptr_incr))
      })

    {:reply, state, state}
  end

  # Pads the int-command with zeroes
  @spec pad(instruction()) :: binary()
  defp pad(instruction) do
    instruction
    |> to_string()
    |> String.pad_leading(5, "0")
  end

  # Extracts command and parameter from a frame of memory
  @spec tokenize(memory()) :: [padded_instruction() | memory()]
  defp tokenize([99 | _]), do: List.wrap(pad(99))
  defp tokenize([x, a | _]) when x in @bins, do: [pad(x), a]
  defp tokenize([x, a, b, c | _]), do: [pad(x), a, b, c]
  defp tokenize(_), do: raise("Invalid opcode")

  # Exexcutes a command from a `instruction_vector`
  defp execute(["00001", a, b, x], state, output) do
    {List.replace_at(state, x, Enum.at(state, a) + Enum.at(state, b)), output}
  end

  defp execute(["00101", a, b, x], state, output) do
    {List.replace_at(state, x, a + Enum.at(state, b)), output}
  end

  defp execute(["01001", a, b, x], state, output) do
    {List.replace_at(state, x, Enum.at(state, a) + b), output}
  end

  defp execute(["01101", a, b, x], state, output) do
    {List.replace_at(state, x, a + b), output}
  end

  defp execute(["00002", a, b, x], state, output) do
    {List.replace_at(state, x, Enum.at(state, a) * Enum.at(state, b)), output}
  end

  defp execute(["00102", a, b, x], state, output) do
    {List.replace_at(state, x, a * Enum.at(state, b)), output}
  end

  defp execute(["01002", a, b, x], state, output) do
    {List.replace_at(state, x, Enum.at(state, a) * b), output}
  end

  defp execute(["01102", a, b, x], state, output) do
    {List.replace_at(state, x, a * b), output}
  end

  defp execute(["00004", x], state, output), do: {state, [Enum.at(state, x) | output]}
  defp execute(["00104", x], state, output), do: {state, [x | output]}
end
