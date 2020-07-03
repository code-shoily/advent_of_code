defmodule AdventOfCode.Y2016.Day2Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2016.Day2, as: Solution

  test "Year 2016, Day 2, Part 1" do
    assert Solution.run_1() == 76_792
  end

  test "run_cmds" do
    matrix = Solution.to_matrix_map()
    assert Solution.run_cmds(matrix, ["U", "L", "L"], 2, 0) == {"5", 2, 0}
    assert Solution.run_cmds(matrix, ["R", "R", "D", "D", "D"], 2, 0) == {"D", 4, 2}
    assert Solution.run_cmds(matrix, ["L", "U", "R", "D", "L"], 4, 2) == {"B", 3, 2}
    assert Solution.run_cmds(matrix, ["U", "U", "U", "U", "D"], 3, 2) == {"3", 1, 2}
  end

  test "Year 2016, Day 2, Part 2" do
    assert Solution.run_2() == "A7AC3"
  end
end
