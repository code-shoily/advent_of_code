defmodule AdventOfCode.Y2020.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2007

  alias AdventOfCode.Y2020.Day07, as: Solution

  test "Year 2020, Day 7" do
    assert Solution.run() == {355, 5312}
  end
end
