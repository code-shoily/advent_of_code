defmodule AdventOfCode.Y2021.Day09Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2109

  alias AdventOfCode.Y2021.Day09, as: Solution

  test "Year 2021, Day 9" do
    assert Solution.run() == {528, 920_448}
  end
end
