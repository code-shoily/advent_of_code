defmodule AdventOfCode.Y2016.Day04Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1604

  alias AdventOfCode.Y2016.Day04, as: Solution

  test "Year 2016, Day 4, Part 1" do
    assert Solution.run_1() == 158_835
  end

  test "Year 2016, Day 4, Part 2" do
    assert Solution.run_2() == 993
  end
end
