defmodule AdventOfCode.Y2019.Day04Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2019.Day04, as: Solution

  test "Year 2019, Day 4, Part 1" do
    assert Solution.run_1() == 1099
  end

  test "Year 2019, Day 4, Part 2" do
    assert Solution.run_2() == 710
  end
end
