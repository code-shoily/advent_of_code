defmodule AdventOfCode.Y2015.Day24Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1524

  alias AdventOfCode.Y2015.Day24, as: Solution

  test "Year 2015, Day 24 run/1" do
    assert Solution.run() == {10_723_906_903, 74_850_409}
  end
end
