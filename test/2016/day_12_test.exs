defmodule AdventOfCode.Y2016.Day12Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1612

  alias AdventOfCode.Y2016.Day12, as: Solution

  test "Year 2016, Day 12, Part 1" do
    assert Solution.run_1() == 318_077
  end

  test "Year 2016, Day 12, Part 2" do
    assert Solution.run_2() == 9_227_731
  end
end
