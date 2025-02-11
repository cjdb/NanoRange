// nanorange/views/drop_while.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_DROP_WHILE_HPP_INCLUDED
#define NANORANGE_VIEWS_DROP_WHILE_HPP_INCLUDED

#include <nanorange/algorithm/find.hpp>
#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/detail/views/semiregular_box.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>
#include <nanorange/detail/views/view_closure.hpp>

NANO_BEGIN_NAMESPACE

template <typename R, typename Pred>
struct drop_while_view : view_interface<drop_while_view<R, Pred>> {

    static_assert(view<R>);
    static_assert(input_range<R>);
    static_assert(std::is_object_v<Pred>);
    static_assert(indirect_unary_predicate<const Pred, iterator_t<R>>);

    drop_while_view() = default;

    constexpr drop_while_view(R base, Pred pred)
        : base_(std::move(base)),
          pred_(std::move(pred))
    {}

    constexpr R base() const { return base_; }

    constexpr const Pred& pred() const { return *pred_; }

    constexpr auto begin()
    {
        if (!cached_.has_value()) {
            cached_ = ranges::find_if(base_,
                [&p = pred()](auto&& arg)
                {
                    return !nano::invoke(p, std::forward<decltype(arg)>(arg));
                });
        }

        return *cached_;
    }

    constexpr auto end()
    {
        return ranges::end(base_);
    }

private:
    R base_;
    detail::semiregular_box<Pred> pred_;
    std::optional<iterator_t<R>> cached_;
};

template <typename R, typename Pred>
drop_while_view(R&& r, Pred pred) -> drop_while_view<all_view<R>, Pred>;

namespace views {

inline constexpr range_adaptor drop_while =
    []<viewable_range R, predicate<range_value_t<R>> Pred>(R&& r, Pred&& pred) {
        return drop_while_view(std::forward<R>(r), std::forward<Pred>(pred));
    };

}

NANO_END_NAMESPACE

#endif
