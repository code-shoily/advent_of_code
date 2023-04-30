defmodule AdventOfCode.Y2020.Day05Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2005

  alias AdventOfCode.Y2020.Day05, as: Solution

  test "Year 2020, Day 5" do
    assert Solution.run() == {930, 515}
  end
end
