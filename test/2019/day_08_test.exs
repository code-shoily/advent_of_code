defmodule AdventOfCode.Y2019.Day08Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1908

  alias AdventOfCode.Y2019.Day08, as: Solution

  @output """
  ▒  ▒ ▒   ▒▒  ▒ ▒▒▒▒ ▒▒▒▒
  ▒ ▒  ▒   ▒▒  ▒ ▒    ▒
  ▒▒    ▒ ▒ ▒▒▒▒ ▒▒▒  ▒▒▒
  ▒ ▒    ▒  ▒  ▒ ▒    ▒
  ▒ ▒    ▒  ▒  ▒ ▒    ▒
  ▒  ▒   ▒  ▒  ▒ ▒    ▒▒▒▒
  """

  test "Year 2019, Day 8" do
    {solution_1, solution_2} = Solution.run()
    assert solution_1 == 1572
    assert strip_spaces(solution_2) == strip_spaces(@output)
  end

  defp strip_spaces(lines) do
    lines
    |> String.split("\n", trim: true)
    |> Enum.map_join("\n", &String.trim_trailing(&1, " "))
  end
end
