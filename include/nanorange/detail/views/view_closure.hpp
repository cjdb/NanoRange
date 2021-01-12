#ifndef NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP
#define NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

#include <tuple>
#include <type_traits>

NANO_BEGIN_NAMESPACE
template<copy_constructible F>
class range_adaptor;

template<typename F, typename R, typename... Args>
concept __pipeable =
	viewable_range<R> and invocable<F, R, Args...> and view<std::invoke_result_t<F, R, Args...>>;

template<typename T, typename U>
concept __decays_to = same_as<std::decay_t<T>, U>;

template<typename>
inline constexpr auto __is_range_adaptor_v = false;

template<typename F>
inline constexpr auto __is_range_adaptor_v<range_adaptor<F>> = true;

template<typename T>
concept __is_range_adaptor = __is_range_adaptor_v<std::remove_cvref_t<T>>;

template<copy_constructible F>
class range_adaptor : F {
public:
	constexpr range_adaptor(F&& f) noexcept(std::is_nothrow_move_constructible_v<F>)
	: F(std::move(f))
	{}

	template<input_range R, typename... Args>
	requires __pipeable<F&, R, Args...>
	[[nodiscard]] constexpr auto operator()(R&& r, Args&&... args) const noexcept(std::is_nothrow_invocable_v<F&, R, Args...>)
	{
		return std::invoke(static_cast<F const&>(*this), std::forward<R>(r), std::forward<Args>(args)...);
	}

	template<typename... Args>
	[[nodiscard]] constexpr auto operator()(Args&&... args) const
	{
		return nano::ranges::range_adaptor(
		   [this, ...args = std::forward<Args>(args)]<viewable_range R>(R&& r) {
			   return (*this)(std::forward<R>(r), args...);
		   });
	}

	template<input_range R, __decays_to<range_adaptor> A>
	requires __pipeable<F&, R>
	[[nodiscard]] constexpr friend auto operator|(R&& r, A&& adaptor) noexcept(std::is_nothrow_invocable_v<F&, R>)
	{
		return std::forward<A>(adaptor)(std::forward<R>(r));
	}

	template<__decays_to<range_adaptor> T, __is_range_adaptor U>
	[[nodiscard]] constexpr friend auto operator|(T&& x, U&& y)
	{
		return nano::ranges::range_adaptor(
		   [x = std::forward<T>(x), y = std::forward<U>(y)]<viewable_range R>(R&& r) {
			   return std::invoke(y, std::invoke(x, std::forward<R>(r)));
		   });
	}

	friend void operator|(auto&&, range_adaptor const&&) = delete;
	friend void operator|(range_adaptor const&&, auto&&) = delete;
};

template<typename F>
range_adaptor(F closure) -> range_adaptor<F>;
NANO_END_NAMESPACE

#endif // NANORANGE_DETAIL_VIEWS_VIEW_CLOSURE_HPP
