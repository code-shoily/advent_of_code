defmodule AdventOfCode.Y2019.Day05Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1905

  alias AdventOfCode.Y2019.Day05, as: Solution

  test "Year 2019, Day 5" do
    assert Solution.run() == {6_745_903, nil}
  end
end
