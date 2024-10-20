defmodule AdventOfCode.Y2020.Day10Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2010

  alias AdventOfCode.Y2020.Day10, as: Solution

  test "Year 2020, Day 10" do
    assert Solution.run() == {2030, 42_313_823_813_632}
  end
end
