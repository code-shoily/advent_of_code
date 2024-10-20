defmodule AdventOfCode.Y2015.Day18Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1518

  alias AdventOfCode.Y2015.Day18, as: Solution

  test "Year 2015, Day 18" do
    assert Solution.run() == {814, 924}
  end
end
