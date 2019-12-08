defmodule AdventOfCode.Y2019.IntCode do
  use GenServer

  @store "00003"
  @bins [3, 4, 104]
  @halt "00099"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def get_state(pid), do: GenServer.call(pid, :state)

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
       next: extract(data),
       iptr: 0,
       output: [],
       start_val: 1
     }}
  end

  @impl true
  def handle_call(:state, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call({:cmd, [@halt | _]}, _from, state) do
    new_state =
      state
      |> Map.put(:iptr, :halt)

    {:reply, new_state, new_state}
  end

  @impl true
  def handle_call({:cmd, [@store, addr]}, _from, %{start_val: start_val, data: data} = state) do
    new_data = List.replace_at(data, addr, start_val)

    new_state =
      state
      |> Map.put(:data, new_data)
      |> Map.update(:iptr, 0, fn iptr -> iptr + 2 end)
      |> Map.put(:next, new_data |> Enum.drop(2) |> extract())

    {:reply, new_state, new_state}
  end

  @impl true
  def handle_call({:cmd, vector}, _from, %{data: data, output: output, iptr: iptr} = state) do
    {new_data, new_output} = execute(vector, data, output)
    ptr_incr = iptr + length(vector)

    new_state =
      state
      |> Map.merge(%{
        data: new_data,
        iptr: ptr_incr,
        output: new_output,
        next: new_data |> Enum.drop(ptr_incr) |> extract()
      })

    {:reply, new_state, new_state}
  end

  defp pad_zeroes(number), do: number |> to_string() |> String.pad_leading(5, "0")

  defp extract([99 | _]), do: [pad_zeroes(99)]
  defp extract([x, a | _]) when x in @bins, do: [pad_zeroes(x), a]
  defp extract([x, a, b, c | _]), do: [pad_zeroes(x), a, b, c]
  defp extract(_), do: raise("Invalid opcode")

  defp execute(["00001", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) + Enum.at(state, b)
       ), output}

  defp execute(["00101", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a + Enum.at(state, b)
       ), output}

  defp execute(["01001", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) + b
       ), output}

  defp execute(["01101", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a + b
       ), output}

  defp execute(["00002", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) * Enum.at(state, b)
       ), output}

  defp execute(["00102", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a * Enum.at(state, b)
       ), output}

  defp execute(["01002", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) * b
       ), output}

  defp execute(["01102", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a * b
       ), output}

  defp execute(["00004", x], state, output),
    do: {state, [Enum.at(state, x) | output]}

  defp execute(["00104", x], state, output), do: {state, [x | output]}
end
