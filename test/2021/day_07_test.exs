defmodule AdventOfCode.Y2021.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2107

  alias AdventOfCode.Y2021.Day07, as: Solution

  test "Year 2021, Day 7" do
    assert Solution.run() == {344_138, 94_862_124}
  end
end
