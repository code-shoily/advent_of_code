defmodule AdventOfCode.Y2017.Day05Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1705

  alias AdventOfCode.Y2017.Day05, as: Solution

  test "Year 2017, Day 5" do
    assert Solution.run() == {372_671, 25_608_480}
  end
end
