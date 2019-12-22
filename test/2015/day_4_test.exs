defmodule AdventOfCode.Y2015.Day4Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2015.Day4, as: Solution

  test "Year 2015, Day 4, Part 1" do
    assert Solution.run_1() == 254_575
  end

  test "Year 2015, Day 4, Part 2" do
    assert Solution.run_2() == "bgvyzdsv"
  end
end
