defmodule AdventOfCode.Y2016.Day24Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1624

  alias AdventOfCode.Y2016.Day24, as: Solution

  test "Year 2016, Day 24 run/1" do
    assert Solution.run() == {462, 676}
  end
end
