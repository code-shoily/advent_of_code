defmodule AdventOfCode.Y2018.Day3Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2018.Day3, as: Solution

  test "Year 2018, Day 3, Part 1" do
    assert Solution.run_1() == 110_389
  end

  test "Year 2018, Day 3, Part 2" do
    assert Solution.run_2() == 552
  end
end
