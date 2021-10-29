defmodule AdventOfCode.Y2016.Day05Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1605_slow

  alias AdventOfCode.Y2016.Day05, as: Solution

  @tag :skip_slow
  test "Year 2016, Day 5, Part 1" do
    assert Solution.run_1() == "f77a0e6e"
  end

  # @tag timeout: 120_000
  @tag :skip_slow
  test "Year 2016, Day 5, Part 2" do
    assert Solution.run_2() == "999828ec"
  end
end
