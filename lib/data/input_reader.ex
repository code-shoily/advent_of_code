defmodule AdventOfCode.Data.InputReader do
  def read!(year, day) do
    year |> build_path(day) |> File.read!()
  end

  defp build_path(year, day), do: "#{__DIR__}/inputs/#{year}_#{day}.txt"
end
