defmodule AdventOfCode.Y2015.Day3Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2015.Day3, as: Solution

  test "Year 2015, Day 3, Part 1" do
    assert Solution.run_1() == 2081
  end

  test "Year 2015, Day 3, Part 2" do
    assert Solution.run_2() == 2341
  end
end
