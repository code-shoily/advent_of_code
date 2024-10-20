defmodule AdventOfCode.Y2018.Day01Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1801

  alias AdventOfCode.Y2018.Day01, as: Solution

  test "Year 2018, Day 1" do
    assert Solution.run() == {590, 83_445}
  end
end
