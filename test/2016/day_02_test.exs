defmodule AdventOfCode.Y2016.Day02Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1602

  alias AdventOfCode.Y2016.Day02, as: Solution

  test "Year 2016, Day 2" do
    assert Solution.run() == {76_792, "A7AC3"}
  end

  test "run_cmds" do
    matrix = Solution.to_matrix_map()
    assert Solution.run_cmds(matrix, ["U", "L", "L"], 2, 0) == {"5", 2, 0}
    assert Solution.run_cmds(matrix, ["R", "R", "D", "D", "D"], 2, 0) == {"D", 4, 2}
    assert Solution.run_cmds(matrix, ["L", "U", "R", "D", "L"], 4, 2) == {"B", 3, 2}
    assert Solution.run_cmds(matrix, ["U", "U", "U", "U", "D"], 3, 2) == {"3", 1, 2}
  end
end
