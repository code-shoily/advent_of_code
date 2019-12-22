defmodule AdventOfCode.Y2016.Day2Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2016.Day2, as: Solution

  test "Year 2016, Day 2, Part 1" do
    assert Solution.run_1() == 76792
  end

  test "Year 2016, Day 2, Part 2" do
    assert Solution.run_2() == "A7AC3"
  end
end
