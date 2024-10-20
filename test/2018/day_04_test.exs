defmodule AdventOfCode.Y2018.Day04Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1804

  alias AdventOfCode.Y2018.Day04, as: Solution

  test "Year 2018, Day 4" do
    assert Solution.run() == {74_743, 132_484}
  end
end
