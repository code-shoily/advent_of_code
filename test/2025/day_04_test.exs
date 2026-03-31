defmodule AdventOfCode.Y2025.Day04Test do
  use ExUnit.Case

  alias AdventOfCode.Y2025.Day04, as: Solution
  @moduletag :y2504

  @example """
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.
  """

  test "Part 1 example" do
    rolls = Solution.parse(@example)
    assert Solution.run_1(rolls) == 13
  end

  test "Part 2 example" do
    rolls = Solution.parse(@example)
    assert Solution.run_2(rolls) == 43
  end

  test "Year 2025, Day 4 run/1" do
    assert Solution.run() == {1395, 8451}
  end
end
