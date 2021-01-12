// nanorange/detail/ranges/range_concept.hpp
//
// Copyright (c) 2020 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_RANGE_CONCEPT_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_RANGE_CONCEPT_HPP_INCLUDED

#include <nanorange/detail/ranges/begin_end.hpp>

NANO_BEGIN_NAMESPACE

template <typename T>
concept range = requires(T& t) {
    ranges::begin(t);
    ranges::end(t);
};

NANO_END_NAMESPACE

#endif
