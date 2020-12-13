defmodule AdventOfCode.Y2020.Day13Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2013

  alias AdventOfCode.Y2020.Day13, as: Solution

  test "Year 2020, Day 13, Part 1" do
    assert Solution.run_1() == 4782
  end

  test "Year 2020, Day 13, Part 2" do
    assert Solution.run_2() == nil
  end
end
