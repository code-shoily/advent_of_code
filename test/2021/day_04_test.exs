defmodule AdventOfCode.Y2021.Day04Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2104

  alias AdventOfCode.Y2021.Day04, as: Solution

  test "Year 2021, Day 4, Part 1" do
    assert Solution.run_1() == 11_774
  end

  test "Year 2021, Day 4, Part 2" do
    assert Solution.run_2() == 4495
  end
end
