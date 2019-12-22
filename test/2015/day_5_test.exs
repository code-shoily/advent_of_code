defmodule AdventOfCode.Y2015.Day5Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2015.Day5, as: Solution

  test "Year 2015, Day 5, Part 1" do
    assert Solution.run_1() == 255
  end

  test "Year 2015, Day 5, Part 2" do
    assert Solution.run_2() == 55
  end
end
