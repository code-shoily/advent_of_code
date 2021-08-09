defmodule AdventOfCode.Y2020.Day04Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2004

  alias AdventOfCode.Y2020.Day04, as: Solution

  test "Year 2020, Day 4, Part 1" do
    assert Solution.run_1() == 233
  end

  test "Year 2020, Day 4, Part 2" do
    assert Solution.run_2() == 111
  end
end
