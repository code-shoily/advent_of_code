defmodule AdventOfCode.Y2015.Day04Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1504

  alias AdventOfCode.Y2015.Day04, as: Solution

  test "Year 2015, Day 4, Part 1" do
    assert Solution.run_1() == 254_575
  end

  test "Year 2015, Day 4, Part 2" do
    assert Solution.run_2() == 1_038_736
  end
end
