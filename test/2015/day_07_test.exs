defmodule AdventOfCode.Y2015.Day07Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1507

  alias AdventOfCode.Y2015.Day07, as: Solution

  test "Year 2015, Day 7" do
    assert Solution.run() == {46_065, 14_134}
  end
end
