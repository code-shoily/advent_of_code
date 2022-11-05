defmodule AdventOfCode.Y2015.Day21Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1521

  alias AdventOfCode.Y2015.Day21, as: Solution

  test "Year 2015, Day 21 run/1" do
    assert Solution.run() == {111, 188}
  end
end
