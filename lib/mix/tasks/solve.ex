defmodule Mix.Tasks.Solve do
  def run(args) do
    case AdventOfCode.Helpers.InputParser.parse(args) do
      nil ->
        IO.puts("ERROR!")

      result ->
        result
        |> AdventOfCode.Helpers.FileWriter.write()
        |> IO.puts()
    end
  end
end