defmodule AdventOfCode.Y2015.Day1 do
  def floor(""), do: 0
  def floor("(" <> rest), do: 1 + floor(rest)
  def floor(")" <> rest), do: -1 + floor(rest)

  defp input!(), do: AdventOfCode.Data.InputReader.read!(2015, 1)

  def run do
    input!() |> floor()
  end
end
