defmodule AdventOfCode.Y2015.Day06Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1506_slow

  alias AdventOfCode.Y2015.Day06, as: Solution

  test "Year 2015, Day 6, Part 1" do
    assert Solution.run_1() == 377_891
  end

  test "Year 2015, Day 6, Part 2" do
    assert Solution.run_2() == 14_110_788
  end
end
