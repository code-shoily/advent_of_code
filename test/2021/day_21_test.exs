defmodule AdventOfCode.Y2021.Day21Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2121

  alias AdventOfCode.Y2021.Day21, as: Solution

  test "Year 2021, Day 21, Part 1" do
    assert Solution.run_1() == 1_196_172
  end

  test "Year 2021, Day 21, Part 2" do
    assert Solution.run_2() == 106_768_284_484_217
  end
end
