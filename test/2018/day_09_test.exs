defmodule AdventOfCode.Y2018.Day09Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1809

  alias AdventOfCode.Y2018.Day09, as: Solution

  test "Year 2018, Day 9 run/1" do
    assert Solution.run() == {371_284, 3_038_972_494}
  end
end
