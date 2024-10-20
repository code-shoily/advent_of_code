defmodule AdventOfCode.Y2021.Day21Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2121

  alias AdventOfCode.Y2021.Day21, as: Solution

  test "Year 2021, Day 21" do
    assert Solution.run() == {1_196_172, 106_768_284_484_217}
  end
end
