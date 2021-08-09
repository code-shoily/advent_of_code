defmodule AdventOfCode.Y2018.Day04Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1804

  alias AdventOfCode.Y2018.Day04, as: Solution

  test "Year 2018, Day 4, Part 1" do
    assert Solution.run_1() == 74_743
  end

  test "Year 2018, Day 4, Part 2" do
    assert Solution.run_2() == 132_484
  end
end
