defmodule AdventOfCode.Y2016.Day03Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1603

  alias AdventOfCode.Y2016.Day03, as: Solution

  test "Year 2016, Day 3, Part 1" do
    assert Solution.run_1() == 993
  end

  test "Year 2016, Day 3, Part 2" do
    assert Solution.run_2() == 1849
  end
end
