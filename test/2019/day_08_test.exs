defmodule AdventOfCode.Y2019.Day08Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1908

  alias AdventOfCode.Y2019.Day08, as: Solution

  defp strip_spaces(lines) do
    lines
    |> String.split("\n", trim: true)
    |> Enum.map_join("\n", &String.trim_trailing(&1, " "))
  end

  test "Year 2019, Day 8" do
    assert Solution.run() == {1572, :ok}
  end
end
