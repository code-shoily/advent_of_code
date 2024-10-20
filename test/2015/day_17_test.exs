defmodule AdventOfCode.Y2015.Day17Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1517

  alias AdventOfCode.Y2015.Day17, as: Solution

  test "Year 2015, Day 17, Both" do
    assert Solution.run() == {1304, 18}
  end
end
