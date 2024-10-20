defmodule AdventOfCode.Y2020.Day02Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2002

  alias AdventOfCode.Y2020.Day02, as: Solution

  test "Year 2020, Day 2" do
    assert Solution.run() == {607, 321}
  end
end
