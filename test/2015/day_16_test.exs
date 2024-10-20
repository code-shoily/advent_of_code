defmodule AdventOfCode.Y2015.Day16Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1516

  alias AdventOfCode.Y2015.Day16, as: Solution

  test "Year 2015, Day 16, Both" do
    assert Solution.run() == {213, 323}
  end
end
