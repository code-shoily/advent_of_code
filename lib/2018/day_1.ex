defmodule AdventOfCode.Y2018.Day1 do
  use AdventOfCode.Data.InputReader, year: 2018, day: 1

  def process() do
    input!()
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
  end

  def run_1() do
    Agent.start(fn -> MapSet.new() end, name: __MODULE__)

    result =
      process()
      |> Enum.reduce_while(0, fn acc, x ->
        new_freq = acc + x

        if Agent.get(__MODULE__, fn items -> new_freq in items end) do
          {:halt, new_freq}
        else
          Agent.update(__MODULE__, &MapSet.put(&1, new_freq))
        end

        {:cont, new_freq}
      end)

    result
  end
end
