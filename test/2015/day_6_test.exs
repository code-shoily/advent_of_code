defmodule AdventOfCode.Y2015.Day6Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y156

  alias AdventOfCode.Y2015.Day6, as: Solution

  test "Year 2015, Day 6, Part 1" do
    assert Solution.run_1() == 377_891
  end

  test "Year 2015, Day 6, Part 2" do
    assert Solution.run_2() == 14_110_788
  end
end
