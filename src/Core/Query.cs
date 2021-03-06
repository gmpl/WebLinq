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
    using MoreLinq;

    public static partial class Query
    {
        public static IQuery<TResult> Bind<T, TResult>(this IQuery<T> query, Func<QueryResult<T>, IQuery<TResult>> func) =>
            Create(context =>
            {
                var result = query.GetResult(context);
                var q = func(result);
                return q.GetResult(context);
            });

        public static IQuery<T> Do<T>(this IQuery<T> query, Action<T> action) =>
            query.Select(e => { action(e); return e; });

        public static IQuery<T> Wait<T>(this IQuery<T> query) =>
            query.Bind(xs => Return(xs.ToArray()));

       public static IQuery<int> Sequence(int first, int last) =>
            Sequence(first, last, 1);

        public static IQuery<int> Sequence(int first, int last, int step)
        {
            if (step <= 0)
                throw new ArgumentException(null, nameof(step));
            if (last < first)
                step = -step;
            return MoreEnumerable.Generate(first, i => i + step)
                                 .TakeWhile(i => step < 0 ? i >= last : i <= last)
                                 .ToQuery();
        }

        public static IQuery<T> ToQuery<T>(this IEnumerable<T> items) =>
            Create(context => QueryResult.Create(from item in items select QueryResultItem.Create(context, item)));

        public static IQuery<QueryContext> GetContext() =>
            Create(context => QueryResult.Singleton(context, context));

        public static IQuery<QueryContext> SetContext(QueryContext newContext) =>
            SetContext(_ => newContext);

        public static IQuery<QueryContext> SetContext(Func<QueryContext, QueryContext> contextor) =>
            Create(context => QueryResult.Singleton(contextor(context), context));

        public static IQuery<T> Create<T>(Func<QueryContext, QueryResult<T>> func) =>
            new Query<T>(func);

        public static IQuery<T> Create<T>(Func<QueryContext, IEnumerable<QueryResultItem<T>>> func) =>
            new Query<T>(context => QueryResult.Create(func(context)));

        public static IQuery<T> Singleton<T>(T item) => Array(item);

        public static IQuery<T> Array<T>(params T[] items) => items.ToQuery();

        public static IQuery<T> Return<T>(IEnumerable<T> items) => items.ToQuery();
        public static IQuery<T> Return<T>(IEnumerable<QueryResultItem<T>> items) =>
            Create(context => QueryResult.Create(items));

        public static IEnumerable<T> ToEnumerable<T>(this IQuery<T> query, Func<QueryContext> contextFactory)
        {
            // ReSharper disable once LoopCanBeConvertedToQuery
            foreach (var e in query.GetResult(contextFactory()))
                yield return e.Value;
        }

        public static IQuery<T> FindService<T>() where T : class =>
            from context in GetContext()
            select (IServiceProvider) context into sp
            select (T)sp.GetService(typeof(T));

        public static IQuery<T> GetItem<T>(string key) =>
            from x in TryGetItem(key, (bool found, T value) => new { Found = found, Value = value })
            where x.Found
            select x.Value;

        public static IQuery<TResult> TryGetItem<T, TResult>(string key, Func<bool, T, TResult> resultSelector) =>
            from context in GetContext()
            select context.Items.TryGetValue(key, (some, value) => some ? resultSelector(true, (T) value)
                                                                        : resultSelector(false, default(T)));

        public static IQuery<Unit> SetItem<T>(string key, T value) =>
            SetItem(key, value, delegate { return new Unit(); });

        public static IQuery<TResult> SetItem<T, TResult>(string key, T value, Func<bool, T, TResult> resultSelector) =>
            from ov in TryGetItem(key, resultSelector)
            from _ in SetContext(context => context.WithItems(context.Items.Set(key, value))).Ignore()
            select ov;

        public static IQuery<T> GetService<T>() where T : class =>
            from context in GetContext()
            select context.GetService<T>();

        public static IQuery<T> SetService<T>(T service) where T : class =>
            from current in FindService<T>()
            from _ in SetContext(context => context.WithServiceProvider(context.CacheServiceQueries().LinkService(typeof(T), service))).Ignore()
            select current;

        public static IQuery<Unit> Ignore<T>(this IQuery<T> query) =>
            from _ in query select new Unit();

        public static IQuery<T> Generate<T>(T seed, Func<T, IQuery<T>> generator) =>
            Create(context => QueryResult.Create(GenerateCore(context, seed, generator)));

        static IEnumerable<QueryResultItem<T>> GenerateCore<T>(QueryContext context, T seed, Func<T, IQuery<T>> generator)
        {
            yield return QueryResultItem.Create(context, seed);
            var current = seed;
            while (true)
            {
                foreach (var next in generator(current).GetResult(context))
                {
                    yield return next;
                    current = next.Value;
                    context = next.Context;
                }
            }
        }
    }
}
