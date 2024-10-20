defmodule AdventOfCode.Y2015.Day01Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1501

  alias AdventOfCode.Y2015.Day01, as: Solution

  test "Year 2015, Day 1" do
    assert Solution.run() == {232, 1783}
  end
end
