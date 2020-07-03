defmodule Mix.Tasks.Solve do
  @moduledoc false

  alias AdventOfCode.Helpers.{FileWriter, InputParser}

  def run(args) do
    case InputParser.parse(args) do
      nil ->
        IO.puts("ERROR!")

      result ->
        result
        |> FileWriter.write()
        |> IO.puts()
    end
  end
end
