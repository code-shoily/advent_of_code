defmodule AdventOfCode.Y2015.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1508

  alias AdventOfCode.Y2015.Day08, as: Solution

  test "Year 2015, Day 8, Both Parts" do
    assert Solution.run() == {1333, 2046}
  end
end
