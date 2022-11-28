defmodule AdventOfCode.Y2015.Day05Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1505

  alias AdventOfCode.Y2015.Day05, as: Solution

  test "Year 2015, Day 5" do
    assert Solution.run() == {255, 55}
  end
end
