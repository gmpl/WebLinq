#region Copyright (c) 2016 Atif Aziz. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
#endregion

namespace WebLinq
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    // LINQ support

    static partial class Query
    {
        static IQuery<TReturn> LiftEnumerable<T, TReturn>(this IQuery<T> query, Func<IEnumerable<QueryResultItem<T>>, IEnumerable<QueryResultItem<TReturn>>> func) =>
            query.Bind(xs => Return(func(xs)));

        public static IQuery<TResult> Select<T, TResult>(this IQuery<T> query, Func<T, TResult> selector) =>
           query.LiftEnumerable(xs => from x in xs
                                      select x.WithValue(selector(x.Value)));

        public static IQuery<T> Where<T>(this IQuery<T> query, Func<T, bool> predicate) =>
            query.LiftEnumerable(xs => from x in xs
                                       where predicate(x.Value)
                                       select x);

        public static IQuery<TResult> SelectMany<T1, T2, TResult>(this IQuery<T1> query, Func<T1, IEnumerable<T2>> f, Func<T1, T2, TResult> g) =>
            query.Bind(xs => Create(context => QueryResult.Create(SelectManyIterator(context, xs, f, g))));

        static IEnumerable<QueryResultItem<TResult>> SelectManyIterator<T1, T2, TResult>(QueryContext context, QueryResult<T1> xs, Func<T1, IEnumerable<T2>> f, Func<T1, T2, TResult> g) =>
            from x in xs
            from result in f(x.Value)
            select QueryResultItem.Create(x.Context, g(x.Value, result));

        public static IQuery<TResult> SelectMany<T1, T2, TResult>(this IQuery<T1> query, Func<T1, IQuery<T2>> f, Func<T1, T2, TResult> g) =>
            query.Bind(xs => Create(context => QueryResult.Create(SelectManyIterator(context, xs, f, g))));

        static IEnumerable<QueryResultItem<TResult>> SelectManyIterator<T1, T2, TResult>(QueryContext context, QueryResult<T1> xs, Func<T1, IQuery<T2>> f, Func<T1, T2, TResult> g) =>
            from x in xs
            from result in f(x.Value).GetResult(x.Context)
            select QueryResultItem.Create(result.Context, g(x, result.Value));

        public static IQuery<TResult> Aggregate<T, TState, TResult>(this IQuery<T> query, TState seed,
            Func<TState, T, TState> accumulator,
            Func<TState, TResult> resultSelector)
        {
            if (accumulator == null) throw new ArgumentNullException(nameof(accumulator));
            if (resultSelector == null) throw new ArgumentNullException(nameof(resultSelector));

            return from context in GetContext()
                   select query.GetResult(context).Select(e => e.Value)
                                                  .Aggregate(seed, accumulator, resultSelector);
        }

        public static IQuery<T> SkipWhile<T>(this IQuery<T> query, Func<T, bool> predicate) =>
            query.LiftEnumerable(xs => xs.SkipWhile(x => predicate(x.Value)));

        public static IQuery<T> TakeWhile<T>(this IQuery<T> query, Func<T, bool> predicate) =>
            query.LiftEnumerable(xs => xs.TakeWhile(x => predicate(x.Value)));

        public static IQuery<T> Skip<T>(this IQuery<T> query, int count) =>
            query.LiftEnumerable(e => e.Skip(count));

        public static IQuery<T> Take<T>(this IQuery<T> query, int count) =>
            query.LiftEnumerable(e => e.Take(count));

        public static IQuery<T> Distinct<T>(this IQuery<T> query) =>
            query.LiftEnumerable(Enumerable.Distinct);

        public static IQuery<T> Distinct<T>(this IQuery<T> query, IEqualityComparer<T> comparer) =>
            query.LiftEnumerable(xs => xs.Distinct(comparer.ContraMap<T, QueryResultItem<T>> (x => x.Value)));

        public static IQuery<T> Concat<T>(this IQuery<T> first, IQuery<T> second) =>
            first.Bind(xs => second.Bind(ys => Return(xs.Concat(ys))));
    }
}
