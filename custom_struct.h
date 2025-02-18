#if !defined(COMSTUM_STRUCT_HEADER)
#define COMSTUM_STRUCT_HEADER

#include <utility>
#include <array>

#include <new> // for launder.
namespace cs::i
{
	template<typename T>
	using remove_cvref_t = ::std::remove_cv_t<::std::remove_reference_t<T>>;
	template<typename...>
	using void_t = void;

	template<::std::size_t index, typename T>
	struct at; // propagate here, out of range or T is not a type list.
	template<::std::size_t index, template<typename...>typename Tp, typename T, typename...Ts>
	struct at<index, Tp<T, Ts...>> 
	{ using type = typename at<index - 1, Tp<Ts...>>::type; };
	template<template<typename...>typename Tp, typename T, typename...Ts>
	struct at<0, Tp<T, Ts...>> { using type = T; };
	template<::std::size_t index, typename T>
	using at_t = typename at<index, T>::type;

	template<::std::size_t index, typename...Ts>
	struct get_type;
	template<::std::size_t index, typename T, typename...Ts>
	struct get_type<index, T, Ts...> { using type = typename get_type<index - 1, Ts...>::type; };
	template<typename T, typename...Ts>
	struct get_type<0, T, Ts...> { using type = T; };

	template<::std::size_t index, typename...Ts>
	using get_type_t = typename get_type<index, Ts...>::type;

	template<typename T>
	struct size;
	template<template<typename...>typename Tp, typename...Ts>
	struct size<Tp<Ts...>> { static constexpr auto value{sizeof...(Ts)}; };

	template<typename T>
	inline constexpr auto size_v{size<T>::value};

	template<typename, typename T>
	struct append;
	template<typename T, template<typename...>typename Tp, typename...Ts>
	struct append<T, Tp<Ts...>> { using type = Tp<Ts..., T>; };
	template<template<typename...>typename Tps, typename...Tas, 
		template<typename...>typename Tpd, typename...Ts>
	struct append<Tps<Tas...>, Tpd<Ts...>> { using type = Tpd<Ts..., Tas...>; };

	template<typename T, typename Ct>
	using append_t = typename append<T, Ct>::type;

	template<::std::size_t num
		, typename T
		, typename = void>
	struct drop;
	template< ::std::size_t num
		, template<typename...>typename Tp
		, typename T, typename...Ts>
	struct drop<num, Tp<T, Ts...>, ::std::enable_if_t<(num != 0), void>> { using type = typename drop<num - 1, Tp<Ts...>>::type; };
	template< template<typename...>typename Tp
		, typename...Ts>
	struct drop<0, Tp<Ts...>> { using type = Tp<Ts...>; };

	template<::std::size_t num, typename T>
	using drop_t = typename drop<num, T>::type;

	template<::std::size_t current, ::std::size_t align>
	inline constexpr auto align_up_v{(current + align - 1) / align * align};

	// placeholder that notify library to skip this parameter.
	inline constexpr struct skip_t {} skip;

	template<typename T, typename Target>
	struct qualify_like { using type = T; };
	template<typename T, typename Target>
	struct qualify_like<T, Target const> { using type = T const; };
	template<typename T, typename Target>
	struct qualify_like<T, Target volatile> { using type = T volatile; };
	template<typename T, typename Target>
	struct qualify_like<T, Target const volatile> { using type = T const volatile; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar&>{ using type = T&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar const&>{ using type = T const&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar volatile&>{ using type = T volatile&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar const volatile&>{ using type = T const volatile&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar&&>{ using type = T&&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar volatile&&>{ using type = T volatile&&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar const&&>{ using type = T const&&; };
	template<typename T, typename Tar>
	struct qualify_like<T, Tar const volatile&&>{ using type = T const volatile&&; };

	template<typename T, typename Target>
	using qualify_like_t = typename qualify_like<T, Target>::type;

	void panic(const char*);

	template<typename...Ts>
	class std_packer
	{
		using self = std_packer;

		template<::std::size_t size>
		static constexpr::std::size_t calsize() noexcept
		{ return size; }
		template<::std::size_t size, typename T, typename...Rs>
		static constexpr::std::size_t calsize() noexcept
		{ return self::template calsize<(align_up_v<size, alignof(T)> + sizeof(T)), Rs...>(); }

		template<::std::size_t index = 0, ::std::size_t offset = 0, ::std::size_t...indices>
		static constexpr auto caloffset( ::std::array<::std::size_t, index> values
										, ::std::index_sequence<indices...>) noexcept
		{
			if constexpr (index == sizeof...(Ts))
				return values;
			else 
				return self::template caloffset
				<index + 1, (align_up_v<offset, alignof(at_t<index, self>)> + sizeof(at_t<index, self>))>
				({values.at(indices)..., offset}, ::std::make_index_sequence<index + 1>{});
		}
		template<::std::size_t index = 0>
		static constexpr auto sel_align(::std::size_t result = 0) // make intellsense happy.
		{
			if constexpr (index == sizeof...(Ts))
				return result;
			else {
				constexpr auto val = alignof(at_t<index, self>);
				return sel_align<index + 1>(result > val ? result : val);
			}
		}
	public:
		template<::std::size_t index>
		static constexpr::std::size_t offset_of(::std::in_place_index_t<index> = ::std::in_place_index<index>) noexcept { return offsets[index]; }
		static constexpr::std::size_t offset_of(::std::size_t index) noexcept { return offsets[index]; }

		static constexpr auto num    {sizeof...(Ts)};
		static constexpr auto size   {self::template calsize<0, Ts...>()};
		static constexpr auto offsets{self::template caloffset<0>({}, ::std::index_sequence<>{})};
		static constexpr auto align  {self::template sel_align<0>()};
	};

	// @tparam Pack: may allow more packer to pack types together.
	template<typename Pack>
	class alignas(Pack::align) tuple
	{
		using self = tuple;
	public:
		using pack = Pack;

	private:
		template< template<typename...>typename Pred
			, ::std::size_t index = 0
			, typename T, typename...Args>
		static constexpr bool do_validate() noexcept
		{
			if constexpr (index == pack::num)
				return true;
			else if constexpr (::std::is_same_v<remove_cvref_t<T>, skip_t>)
				if constexpr (Pred<at_t<index, pack>>::value)
					return self::template do_validate<Pred, index + 1, Args...>();
				else
					return false;
			else if constexpr (::std::is_void_v<T>)
				if constexpr (Pred<at_t<index, pack>>::value)
					return self::template do_validate<Pred, index + 1, T>();
				else
					return false;
			else if constexpr (Pred<at_t<index, pack>, T>::value) 
				return self::template do_validate<Pred, index + 1, Args...>();
			else 
				return false;
		}
		template<template<typename...>typename Pred, typename...Args>
		static constexpr bool require() noexcept 
		{ return self::template do_validate<Pred, 0, Args..., void>(); };

	public: // thank you cpp commitee.
		template<::std::size_t index>
		constexpr auto &&get(::std::in_place_index_t<index> = ::std::in_place_index<index>) && noexcept 
		{ return static_cast<at_t<index, Pack>&&>
			(*::std::launder(reinterpret_cast<at_t<index, Pack>*>(this->v_ + pack::template offset_of<index>()))); }
		template<::std::size_t index>
		constexpr auto &&get(::std::in_place_index_t<index> = ::std::in_place_index<index>) const&& noexcept // remove?
		{ return static_cast<at_t<index, Pack> const&&> 
			(*::std::launder(reinterpret_cast<at_t<index, Pack> const*>(this->v_ + pack::template offset_of<index>()))); }
		template<::std::size_t index>
		constexpr auto &&get(::std::in_place_index_t<index> = ::std::in_place_index<index>) const volatile&& noexcept // remove?
		{ return static_cast<at_t<index, Pack> const volatile&&>
			(*::std::launder(reinterpret_cast<at_t<index, Pack> const volatile*>(this->v_ + pack::template offset_of<index>()))); }

		template<::std::size_t index>
		constexpr auto &get(::std::in_place_index_t<index> = ::std::in_place_index<index>) & noexcept
		{ return*::std::launder(reinterpret_cast<at_t<index, Pack>*>(this->v_ + pack::template offset_of<index>())); }
		template<::std::size_t index>
		constexpr auto &get(::std::in_place_index_t<index> = ::std::in_place_index<index>) const& noexcept
		{ return*::std::launder(reinterpret_cast<at_t<index, Pack> const*>(this->v_ + pack::template offset_of<index>())); }
		template<::std::size_t index>
		constexpr auto &get(::std::in_place_index_t<index> = ::std::in_place_index<index>) volatile& noexcept
		{ return*::std::launder(reinterpret_cast<at_t<index, Pack> volatile*>(this->v_ + pack::template offset_of<index>())); }
		template<::std::size_t index>
		constexpr auto &get(::std::in_place_index_t<index> = ::std::in_place_index<index>) const volatile& noexcept // remove?
		{ return*::std::launder(reinterpret_cast<at_t<index, Pack> const volatile*>(this->v_ + pack::template offset_of<index>())); }

		template<::std::size_t index, typename Self, ::std::enable_if_t<(::std::is_same_v<tuple, remove_cvref_t<Self>>), int> = 0>
		friend constexpr auto &&get(Self &&self, ::std::in_place_index_t<index> = ::std::in_place_index<index>) noexcept
		{ return static_cast<Self&&>(self).template get<index>(); }

	private:
		template< ::std::size_t index = 0
			, typename Ptr
			, typename A
			, typename...Args>
		static constexpr void do_construct(Ptr pvals, A &&a, Args&&...args) 
		{
			if constexpr (index == pack::num) 
				return;
			else {
				if constexpr (::std::is_same_v<remove_cvref_t<A>, skip_t>)
					new (pvals + pack::template offset_of<index>()) 
					qualify_like_t<at_t<index, pack>, ::std::remove_pointer_t<Ptr>> {};
				else
					new (pvals + pack::template offset_of<index>()) 
					qualify_like_t<at_t<index, pack>, ::std::remove_pointer_t<Ptr>> (static_cast<A&&>(a));

				self::template do_construct<index + 1>(pvals, static_cast<Args&&>(args)...);
			}
		}
		template<::std::size_t index = 0, typename Ptr = void>
		static constexpr void do_construct(Ptr pvals) 
		{
			if constexpr (index == pack::num) 
				return;
			else 
				static_cast<void>(new (pvals + pack::template offset_of<index>()) 
								  qualify_like_t<at_t<index, pack>, ::std::remove_pointer_t<Ptr>>{}),
				self::template do_construct<index + 1>(pvals);
		}

		template<::std::size_t index = 0, typename Other = int, typename Ptr = void>
		static constexpr void do_mirgate(Ptr pvals, Other &&other) 
		{ 
			if constexpr (index == pack::num) 
				return;
			else 
				static_cast<void>(new (pvals + pack::template offset_of<index>()) 
								  qualify_like_t<at_t<index, pack>, ::std::remove_pointer_t<Ptr>>
			{static_cast<Other&&>(other).template get<index>()}),
				self::template do_mirgate<index + 1>(pvals, static_cast<Other&&>(other));
		}

		template<::std::size_t index = 0, typename Other = void, typename Self = void>
		static constexpr void do_assign(Self &self, Other &&other) 
		{ 
			if constexpr (index == pack::num) 
				return;
			else 
				static_cast<void>(self.template get<index>() = static_cast<Other&&>(other).template get<index>()),
				self::template do_assign<index + 1>(self, static_cast<Other&&>(other));
		}

		template<::std::size_t index = 0, typename Self = void>
		static constexpr void do_destruct(Self &self) noexcept
		{
			if constexpr (index == pack::num) 
				return;
			else {
				using T = at_t<index, pack>;
				self.template get<index>().~T(),
					self::template do_destruct<index + 1>(self);
			}
		}

	public:
		template< typename...Args
			, ::std::enable_if_t<(self::template require<::std::is_constructible, Args&&...>())
			, int> = 0>
		constexpr tuple(Args&&...args)
			noexcept(self::template require<::std::is_nothrow_constructible, Args&&...>()) /* strength */
		{ self::do_construct(this->v_, static_cast<Args&&>(args)...); }

		~tuple() noexcept { self::do_destruct(*this); }

		constexpr tuple(tuple const &other)
			noexcept(self::template require<::std::is_nothrow_copy_constructible>()) /* strength */
		{ static_assert(self::template require<::std::is_copy_constructible>()); 
		if (&other != this) self::do_mirgate(this->v_, other); }

		constexpr tuple(tuple &&other)
			noexcept(self::template require<::std::is_nothrow_move_constructible>()) /* strength */
		{ static_assert(self::template require<::std::is_move_constructible>()); 
		if (&other != this) self::do_mirgate(this->v_, static_cast<tuple&&>(other)); }

		constexpr tuple& operator=(tuple const &other)
			noexcept(self::template require<::std::is_nothrow_copy_assignable>()) /* strength */
		{ static_assert(self::template require<::std::is_copy_assignable>());
		if (&other != this) self::do_assign(*this, other); 
		return *this; }

		constexpr tuple& operator=(tuple &&other)
			noexcept(self::template require<::std::is_nothrow_move_assignable>()) /* strength */
		{ static_assert(self::template require<::std::is_move_assignable>());
		if (&other != this) self::do_assign(*this, static_cast<tuple&&>(other)); 
		return *this; }

	private:
		char v_[pack::size];
	};

	// standard layout tuple. 
	template<typename...Ts>
	using std_tuple = tuple<std_packer<Ts...>>;

	//template<typename T>
	//struct optional
	//{
	//	static constexpr auto size{sizeof(T) > sizeof(::std::exception_ptr) ? sizeof(T) : sizeof(::std::exception_ptr)};

	//	optional(::std::exception_ptr ptr) : m_{1u} { new(v_)::std::exception_ptr{ptr}; }
	//	~optional() {
	//		if (m_ == 1u)
	//			::std::rethrow_exception();
	//	}

	//private:
	//	char m_;
	//	char v_[size];
	//};
}

//#include <functional>

namespace cs::i
{
	template<typename, typename = void>
	struct affine;
	template<typename T>
	struct affine<T, void_t<typename T::type>> { using type = typename T::type; };
	template<template<typename>typename Tp, typename T>
	struct affine<Tp<T>> { using type = T; };

	template<typename T>
	using affine_t = typename affine<T>::type;

	template<typename T>
	struct size_from_type;
	template<typename T, ::std::size_t size>
	struct size_from_type<T[size]>
	{
		static constexpr auto value{size};
	};
	template<template<typename, ::std::size_t> typename Tp, typename T, ::std::size_t size>
	struct size_from_type<Tp<T, size>>
	{
		static constexpr auto value{size};
	};
	template<template<typename> typename Tp, typename T, ::std::size_t size>
	struct size_from_type<Tp<T[size]>>
	{
		static constexpr auto value{size};
	};
	template<template<typename...>typename Tp, typename...Ts>
	struct size_from_type<Tp<Ts...>>
	{
		static constexpr auto value{sizeof...(Ts)};
	};
	template<typename T>
	inline constexpr auto size_from_type_v{size_from_type<::std::remove_cvref_t<T>>::value};

	template<::std::size_t icm = 1u>
	struct linear_probe
	{
		static_assert(icm != 0); // avoid dead recurison.

		using self = linear_probe;

		template<typename T, typename V, typename Compare>
		constexpr auto operator()(T &&result, ::std::size_t stub, V value, Compare compare) const noexcept
		{ 
			constexpr auto size{size_from_type_v<T>};
			for (auto offset{0u}; (offset % size_from_type_v<T>) != 0; offset += icm) { 
				const auto turn{(stub + offset) % size};
				if (compare(result[turn], value))
					return turn;
			} return size;
		}
	};

	struct always_equal
	{
		template<typename L, typename R>
		constexpr bool operator()(L&&, R&&) const noexcept { return true; }
	};

	struct mod_to_index
	{
		using self = mod_to_index;

		constexpr auto operator()(::std::size_t hc, ::std::size_t mod) const noexcept { return hc % mod; }
	};

	template<typename Probe = linear_probe<>, typename Indexer = mod_to_index>
	struct flat_with
	{
		using self  = flat_with;
		using prber = Probe;
		using idxer = Indexer;

		template<typename T>
		static constexpr auto probe(T &array, ::std::size_t stub, ::std::size_t invalid) noexcept
		{ return prber{}(array, stub, invalid, ::std::equal_to<>{}); }

		static constexpr auto index(::std::size_t hash_code, ::std::size_t size) noexcept
		{ return idxer{}(hash_code, size); }

		template<typename T>
		static constexpr auto next(T &array, ::std::size_t stub, ::std::size_t invalid) noexcept
		{ return prber{}(array, stub, invalid, always_equal{}); }
	};

	template<typename F, typename H, typename...Metas>
		requires(requires{ ((static_cast<void>(Metas::key)), ...); })
	struct basic_hash_smap // member map. (store offset of member)
	{
		using self  = basic_hash_smap;
		using hash  = H;
		using flat  = F;
		using key_t = ::std::remove_cvref_t<decltype(get_type_t<0u, Metas...>::key)>;

		static_assert(::std::default_initializable<key_t>, 
					  "Currently not support type not default initializable take part as key."); 
		// \/ \/ see below \/ \/

		template<::std::size_t turn = 0>
		static constexpr key_t key(::std::size_t index) noexcept // TODO: assume all key are same. 
		{ 
			// TODO: find other way replace it.
		#define MC_CONSTEXPR_GET_KEY_(num) case (num + turn): \
			if constexpr (num + turn < sizeof...(Metas))      \
				return get_type_t<num + turn, Metas...>::key; \
			else return {};                                   \
			break

			switch (index)
			{			
				MC_CONSTEXPR_GET_KEY_(0);
				MC_CONSTEXPR_GET_KEY_(1);
				MC_CONSTEXPR_GET_KEY_(2);
				MC_CONSTEXPR_GET_KEY_(3);
				MC_CONSTEXPR_GET_KEY_(4);
				MC_CONSTEXPR_GET_KEY_(5);
				MC_CONSTEXPR_GET_KEY_(6);
				MC_CONSTEXPR_GET_KEY_(7);
			default:
			if constexpr (turn + 8 < sizeof...(Metas))
				return self::template key<turn + 4>(index);
			else
				return {};
			}

		#undef MC_CONSTEXPR_GET_KEY_
		}

		template<::std::size_t index = 1, typename T = void>
		static constexpr auto pow(T value)
		{
			if constexpr (sizeof(T) * 8 == index)
				return value + 1;
			else
				return self::template pow<(index << 1)>(static_cast<T>(value | (value >> index)));
		}
		static constexpr auto nbins{self::pow(sizeof...(Metas))};

		using bins_t = ::std::array<::std::size_t, nbins>;

		static constexpr auto invalid_value{::std::size_t(-1)};
		template<::std::size_t index = 0>
		static constexpr auto invalid_bins(bins_t result = {})
		{
			if constexpr (index == nbins)
				return::std::move(result);
			else {
				result[index] = invalid_value;
				return self::template invalid_bins<index + 1>(::std::move(result));
			}
		}

		template<::std::size_t index = 0>
		static constexpr auto init_bins(bins_t result)
		{
			if constexpr (index == sizeof...(Metas))
				return::std::move(result);
			else {
				// NOTE: we must obtain hash code at compile time. 
				auto index_to_place{flat::index(hash{}(self::key(index)), nbins)};
				if (result[index_to_place] != invalid_value) {
					// NOTE: Probe must obtain index at compile time.
					auto index_probe = flat::probe(result, index_to_place, invalid_value);
					if (index_to_place == index_probe) {
						panic("Oops! Index collide under specified hash algorthim, try another one.");
						return self::invalid_bins();
					} else 
						index_to_place = index_probe;
				}
				result[index_to_place] = index;
				return self::template init_bins<index + 1>(::std::move(result));
			}
		}

		static constexpr auto bins{self::init_bins(self::invalid_bins())};

		template<typename T>
		static constexpr auto index(T const &value) noexcept
		{
			for (auto i{flat::index(hash{}(value), self::nbins)};
				 i != self::nbins; 
				 i = flat::next(self::bins, i, invalid_value)) {
				auto mapped = self::bins[i];
				if (self::key(mapped) == value)
					return mapped;
				else if (mapped == invalid_value)
					break;
			} return invalid_value;
		}
	};
	template<typename F, typename H, typename T>
	struct basic_hash_smap<F, H, T>
	{
		using key_t = ::std::remove_cvref_t<decltype(T::key)>;
		static constexpr auto invalid_value{size_t(-1)};

		template<typename V>
		static constexpr auto index(V const &value) noexcept
			requires(requires{ T::key == value; }) 
		{
			if (T::key == value)
				return size_t(0u);
			else
				return invalid_value;
		}
	};
	template<typename F, typename H>
	struct basic_hash_smap<F, H>
	{
		using key_t = void;
		static constexpr auto invalid_value{size_t(-1)};

		template<typename V>
		static constexpr auto index(V const &value) noexcept
		{ return invalid_value; }
	};

	struct no_such {};

	template<auto desc>
	struct basic_struct
	{
		using self = basic_struct;
		using pack = decltype(desc(::std::in_place_index<0>));
		using map  = decltype(desc(::std::in_place_index<1>));

		using data = tuple<pack>;

		template<typename...Args>
		constexpr basic_struct(Args&&...args)
			noexcept(::std::is_nothrow_constructible_v<data, Args&&...>)
			requires(::std::constructible_from<data, Args&&...>)
		: d_{static_cast<Args&&>(args)...}
		{}

		template<auto value, typename Self>
		friend decltype(auto) get(Self &&sf) noexcept
			requires(::std::same_as<::std::remove_cvref_t<Self>, self>
		&& map::index(value) != map::invalid_value)
		{ constexpr auto index = map::index(value);
		return static_cast<qualify_like_t<data, Self&&>>(sf.d_).template get<index>(); }

	private:
		data d_;
	};

	namespace d
	{
		template<typename Pks, typename Rs>
		constexpr auto basic_struct_desc = 
			[]<template<::std::size_t>typename Tp, ::std::size_t index>(Tp<index>)
		{
			if constexpr (index == 0)
				return Pks{};
			else if constexpr (index == 1)
				return Rs{};
		};
	}

	template<typename Key, typename...Metas>
	using hash_lmsmap = basic_hash_smap<flat_with<linear_probe<1u>, mod_to_index>, ::std::hash<Key>, Metas...>;
}

namespace cs
{
	template<typename Key, typename...Metas>
	using std_struct = i::basic_struct<
		i::d::basic_struct_desc<
			i::std_packer<i::affine_t<Metas>...>, 
			i::hash_lmsmap<Key, Metas...>>>;
}

#endif