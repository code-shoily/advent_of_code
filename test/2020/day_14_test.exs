defmodule AdventOfCode.Y2020.Day14Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2014

  alias AdventOfCode.Y2020.Day14, as: Solution

  test "Year 2020, Day 14, Part 1" do
    assert Solution.run_1() == 9_967_721_333_886
  end

  test "Year 2020, Day 14, Part 2" do
    assert Solution.run_2() == nil
  end
end
