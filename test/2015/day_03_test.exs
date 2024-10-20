defmodule AdventOfCode.Y2015.Day03Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1503

  alias AdventOfCode.Y2015.Day03, as: Solution

  test "Year 2015, Day 3" do
    assert Solution.run() == {2081, 2341}
  end
end
