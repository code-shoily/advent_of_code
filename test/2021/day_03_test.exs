defmodule AdventOfCode.Y2021.Day03Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2103

  alias AdventOfCode.Y2021.Day03, as: Solution

  test "Year 2021, Day 3, Part 1" do
    assert Solution.run_1() == 1_540_244
  end

  test "Year 2021, Day 3, Part 2" do
    assert Solution.run_2() == 4_203_981
  end
end
