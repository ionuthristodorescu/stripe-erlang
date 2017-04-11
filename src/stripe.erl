%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=2 sw=2 et
-module(stripe).

-export([get_record_id/1]).
-export([token_create/10, token_create_bank/3, token_get_id/1]).
-export([customer_create/4, customer_get/1, customer_update/3, customer_get_id/1, customer_get_card_details/1,
  customer_update_subresource/3]).
-export([account_create/18, account_update/2, account_update_subresource/3, account_get/1,
  account_get_id/1, account_get_email/1, account_get_bank_details/1]).%, customer_get/1, customer_update/3]).
-export([managed_account_charge_customer/8, capture_charge/5]).
-export([charge_customer/4, charge_card/4]).
-export([subscription_update/3, subscription_update/5,
  subscription_update/6, subscription_cancel/2, subscription_cancel/3, subscription_get_details/1]).
-export([customer/1, event/1, invoiceitem/1]).
-export([recipient_create/6, recipient_update/6]).
-export([transfer_create/5, transfer_cancel/1]).
-export([invoiceitem_create/4]).
-export([gen_paginated_url/1, gen_paginated_url/2,
  gen_paginated_url/3, gen_paginated_url/4]).
-export([get_all_customers/0, get_num_customers/1]).
-export([customer_card_create/2, customer_card_delete/2, customer_card_update/11]).
-export([account_bank_account_create/2, account_bank_account_delete/2]).
-export([plans_get/0, plans_get_details/0]).

-include("stripe.hrl").

-define(VSN_BIN, <<"0.8.0">>).
-define(VSN_STR, binary_to_list(?VSN_BIN)).

% Stripe limit for paginated requests, change
% this number if stripe changes it in the future
% See: https://stripe.com/docs/api#pagination
-define(STRIPE_LIST_LIMIT, 100).

%%%--------------------------------------------------------------------
%%% Subscription plans
%%%--------------------------------------------------------------------
-spec plans_get() -> term().
plans_get() ->
  request_plans().

-spec plans_get_details() -> list().
plans_get_details() ->
  case plans_get() of
    PlansList when is_record(PlansList, stripe_list) ->
      F = fun
        (Plan, AccIn) when is_record(Plan, stripe_plan) ->
          AccIn ++ [plan_get_details(Plan)];
        (SomeError, _) ->
          SomeError
      end,
      Plans = PlansList#stripe_list.data,
      {ok, lists:foldl(F, [], Plans)};
    _ ->
      error
  end.

-spec plan_get_details(#stripe_plan{}) -> list().
plan_get_details(#stripe_plan{id = PlanId, amount = Amount, currency = Currency, interval = Interval,
    interval_count = IntervalCount, name = Name, trial_period_days = TrialPeriodDays}) ->
  [PlanId, Amount, atom_to_list(Currency), Interval, IntervalCount, Name, TrialPeriodDays].

-spec subscription_get_details(#stripe_customer{}) -> list().
subscription_get_details(#stripe_customer{subscriptions = Subscriptions}) ->
  case Subscriptions of
    NoSubscriptions when NoSubscriptions == [] orelse NoSubscriptions == undefined -> [];
    _ ->
      F = fun(Subscription) ->
        [Subscription#stripe_subscription.id, atom_to_list(Subscription#stripe_subscription.status),
          Subscription#stripe_subscription.current_period_start, Subscription#stripe_subscription.current_period_end,
          Subscription#stripe_subscription.trial_start, Subscription#stripe_subscription.trial_end,
          Subscription#stripe_subscription.ended_at, Subscription#stripe_subscription.canceled_at,
          Subscription#stripe_subscription.customer, Subscription#stripe_subscription.start,
          Subscription#stripe_subscription.quantity] ++
        plan_get_details(Subscription#stripe_subscription.plan)
      end,
      lists:map(F, Subscriptions)
  end.

%%%--------------------------------------------------------------------
%%% Charging
%%%--------------------------------------------------------------------
-spec charge_customer(price(), currency(), customer_id(), desc()) -> term().
charge_customer(Amount, Currency, Customer, Desc) ->
  charge(Amount, Currency, [{customer, Customer}], Desc).

-spec managed_account_charge_customer(price(), currency(), customer_id(),
    account_id(), desc(), binary(), integer(), boolean()) -> term().
managed_account_charge_customer(Amount, Currency, CustomerSrc, AccountDest, Desc, CardId, ApplicationFeeCents, CaptureFlag) ->
  charge(Amount, Currency,
    case is_binary(CustomerSrc) andalso CustomerSrc /= <<>> andalso CustomerSrc /= <<"">> of
      true -> [{customer, CustomerSrc}];
      _ -> []
    end ++
    [{destination, AccountDest}] ++
    case ApplicationFeeCents of
      null -> [];
      undefined -> [];
      _ -> [{application_fee, ApplicationFeeCents}]
    end ++
    case is_binary(CardId) andalso CustomerSrc /= <<>> andalso CustomerSrc /= <<"">> of
      true -> [{source, CardId}];
      _ -> []
    end ++
    [{capture, CaptureFlag}],
    Desc).

-spec capture_charge(charge_id(), pos_integer(), pos_integer(), email(), desc()) -> term().
capture_charge(ChargeId, Amount, ApplicationFeeCents, ReceiptEmail, StatementDesc) ->
  Fields = [{amount, Amount}, {application_fee, ApplicationFee}] ++
    case ReceiptEmail of 
      undefined -> [];
      RE -> [{receipt_email, ReceiptEmail}]
    end ++ 
    case StatementDesc of 
      undefined -> [];
      SD -> [{statement_descriptor, StatementDesc}]
    end,
  request_run(gen_charge_subresource_url(ChargeId, capture), post, Fields). 

-spec charge_card(price(), currency(), token_id(), desc()) -> term().
charge_card(Amount, Currency, Card, Desc) ->
  charge(Amount, Currency, [{card, Card}], Desc).

-spec charge(price(), currency(),
    %{customer, customer_id()} | {card, token_id()}, {} | {destination, account_id()},
    list(), desc()) -> term().
charge(Amount, Currency, Params, Desc) when Amount > 50 ->
  Fields = [{amount, Amount}, {currency, Currency}] ++
    Params ++
    [{description, Desc}],
  request_charge(Fields).

%%%--------------------------------------------------------------------
%%% Account Creation
%%%--------------------------------------------------------------------
-spec account_create(account_type(), country(), email(), tuple(),
    string(), string(),
    string(), epoch(), string(), string(),
    string(), string(), string(), string(), string(), string(),
    string(), string()) -> term().
account_create(AcctType, Country, Email, {Day, Month, Year}, FirstName, LastName,
    LegalEntityType, Date, IP, BusinessName,
    City, Country2LettersCode, AddressLine1, AddressLine2, ZIP, AddressState,
    BusinessTaxId, SSNLast4) ->
  DefaultFields =
    [
      {country, Country},
      {email, Email}
    ] ++
    case Year of
      Empty when Empty == [] orelse Empty == undefined orelse Empty == null -> [];
      _ ->
        [
          {"legal_entity[dob][day]", Day},
          {"legal_entity[dob][month]", Month},
          {"legal_entity[dob][year]", Year}
        ]
    end ++
    [
      {"legal_entity[first_name]", FirstName},
      {"legal_entity[last_name]", LastName},
      {"legal_entity[type]", LegalEntityType},
      {"tos_acceptance[date]", Date},
      {"tos_acceptance[ip]", IP},
      {"legal_entity[business_name]", BusinessName}
    ] ++
    case Country2LettersCode of
      Empty when Empty == [] orelse Empty == undefined orelse Empty == null -> [];
      _ ->
        [
          {"legal_entity[address][city]", City},
          {"legal_entity[address][country]", Country2LettersCode},
          {"legal_entity[address][line1]", AddressLine1},
          {"legal_entity[address][line2]", AddressLine2},
          {"legal_entity[address][postal_code]", ZIP},
          {"legal_entity[address][state]", AddressState}
        ]
    end ++
    case BusinessTaxId of
      Empty when Empty == [] orelse Empty == undefined orelse Empty == null -> [];
      _ -> [{"legal_entity[business_tax_id]", BusinessTaxId}]
    end ++
    case SSNLast4 of
      Empty when Empty == [] orelse Empty == undefined orelse Empty == null -> [];
      _ -> [{"legat_entity[ssn_last_4]", SSNLast4}]
    end,

  Fields = if
    AcctType == managed -> DefaultFields ++ [{managed, true}];
    AcctType == standalone -> DefaultFields;
    true -> io:format("Error in account create, acct_type = ~p", [AcctType])
  end,
  request_account_create(Fields).

%%%--------------------------------------------------------------------
%%% Account Updating
%%%--------------------------------------------------------------------
-spec account_update(account_id(), list()) -> term().
account_update(AcctId, Fields) ->
  request_account_update(AcctId, Fields).

-spec account_update_subresource(account_id(), desc(), list()) -> term().
account_update_subresource(AcctId, Resource, Fields) ->
  request_account_update_subresource(AcctId, Resource, Fields).

%%%--------------------------------------------------------------------
%%% Account Fetching
%%%--------------------------------------------------------------------
-spec account_get(account_id()) -> term().
account_get(AccountId) ->
  request_account(AccountId).

-spec account_get_id(#stripe_account{}) -> account_id().
account_get_id(StripeAccount) ->
  StripeAccount#stripe_account.id.

-spec account_get_email(#stripe_account{}) -> email().
account_get_email(StripeAccount) ->
  StripeAccount#stripe_account.email.

-spec account_get_bank_details(#stripe_account{}) -> term().
account_get_bank_details(StripeAccount) ->
  F = fun(StripeBankAccount) ->
    #stripe_bank_account{id = Id, currency = _Currency, default_for_currency = _DefaultForCurrency,
      metadata = _Metadata, fingerprint = _Fingerprint, bank_name = BankName,
      last4 = Last4, country = _Country, status = _Status, routing_number = RoutingNumber,
      account = _StripeAccount} = StripeBankAccount,
    {Id, Last4, BankName, RoutingNumber} end,
  lists:map(F, StripeAccount#stripe_account.bank_accounts).

%%%--------------------------------------------------------------------
%%% Account Bank Account Creation - uses the Bank Account API
%%%--------------------------------------------------------------------
-spec account_bank_account_create(token_id(), account_id()) -> term().
account_bank_account_create(BankToken, AccountId) ->
  Fields = [{external_account, BankToken}],
  request_account_update_subresource(AccountId, "external_accounts", Fields).

-spec account_bank_account_delete(bank_account_id(), account_id()) -> term().
account_bank_account_delete(BankAccountId, AccountId) ->
  request_account_delete_subresource(AccountId, "external_accounts", BankAccountId).

%%%--------------------------------------------------------------------
%%% Customer Creation
%%%--------------------------------------------------------------------
-spec customer_create(token_id(), email(), desc(), plan_id()) -> term().
customer_create(Card, Email, Desc, Plan) ->
  Fields = [{card, Card},
    {email, Email},
    {description, Desc}] ++
  case Plan of
    undefined -> [];
    _ -> [{plan, Plan}]
  end,
  request_customer_create(Fields).

%%%--------------------------------------------------------------------
%%% Customer Card Creation - uses the card API
%%%--------------------------------------------------------------------
-spec customer_card_create(token_id(), customer_id()) -> term().
customer_card_create(CardToken, CustomerId) ->
  Fields = [{source, CardToken}],
  % TODO: changed "cards" to "sources" below, which one should we use ... ?
  request_customer_update_subresource(CustomerId, "sources", Fields).

-spec customer_card_delete(card_id(), customer_id()) -> term().
customer_card_delete(CardId, CustomerId) ->
  request_customer_delete_subresource(CustomerId, "sources", CardId).

-spec customer_card_update(card_id(), customer_id(), string(), string(), string(),
  string(), string(), string(), integer(), integer(), string()) -> term().
customer_card_update(CardId, CustomerId, AddrLine1, AddrLine2, City, Zip, State, Country, ExpMonth,
    ExpYear, Name) ->
  Fields = [
    {address_line1, AddrLine1},
    {address_line2, AddrLine2},
    {address_city, City},
    {address_zip, Zip},
    {address_country, Country},
    {address_state, State},
    {exp_month, ExpMonth},
    {exp_year, ExpYear},
    {name, Name}],
  FilteredFields = [{Key, Value} || {Key, Value} <- Fields, Value /= undefined],
  request_customer_update_subresource(CustomerId, "sources" ++ "/" ++ CardId, FilteredFields).

%%%--------------------------------------------------------------------
%%% Customer Fetching
%%%--------------------------------------------------------------------
-spec customer_get(customer_id()) -> term().
customer_get(CustomerId) ->
  request_customer(CustomerId).

-spec customer_get_id(#stripe_customer{}) -> customer_id().
customer_get_id(StripeCustomer) ->
  StripeCustomer#stripe_customer.id.

-spec customer_get_card_details(#stripe_customer{}) -> term().
customer_get_card_details(StripeCustomer) ->
  F = fun(StripeCard) ->
    #stripe_card{id = Id, exp_year = ExpYear, exp_month = ExpMonth, brand = Brand, last4 = Last4,
      name = Name, cvc_check = CVCCheck, address_line1 = AddrLine1, address_line2 = AddrLine2,
      address_city = City, address_zip = Zip, address_state = AddressState, address_country = Country} = StripeCard,
    {Id, ExpYear, ExpMonth, Brand, Last4, Name, CVCCheck,
      AddrLine1, AddrLine2, City, Zip, AddressState, Country}
  end,
  lists:map(F, StripeCustomer#stripe_customer.sources).

%%%--------------------------------------------------------------------
%%% Customer Updating
%%%--------------------------------------------------------------------
-spec customer_update(customer_id(), token_id(), email()) -> term().
customer_update(CustomerId, Token, Email) ->
  Fields = [{"card", Token},
    {"email", Email}],
  request_customer_update(CustomerId, Fields).

-spec customer_update_subresource(customer_id(), desc(), list()) -> term().
customer_update_subresource(CustomerId, Resource, Fields) ->
  request_customer_update_subresource(CustomerId, Resource, Fields).
%%%--------------------------------------------------------------------
%%% Token Generation
%%%--------------------------------------------------------------------
token_create(CardNumber, ExpMon, ExpYr, Cvc,
    Name, Addr1, Addr2, Zip, State, Country) ->
  Fields = [{"card[number]", CardNumber},
    {"card[exp_month]", ExpMon},
    {"card[exp_year]", ExpYr},
    {"card[cvc]", Cvc},
    {"card[name]", Name},
    {"card[address_line1]", Addr1},
    {"card[address_line2]", Addr2},
    {"card[address_zip]", Zip},
    {"card[address_state]", State},
    {"card[address_country]", Country}],
  request_token_create(Fields).

token_create_bank(Country, RoutingNumber, AccountNumber) ->
  Fields = [{"bank_account[country]", Country},
    {"bank_account[routing_number]", RoutingNumber},
    {"bank_account[account_number]", AccountNumber}],
  request_token_create(Fields).

token_get_id(Token) ->
  Token#stripe_token.id.

%%%--------------------------------------------------------------------
%%% subscription updating/creation and removal
%%%--------------------------------------------------------------------
subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd) ->
  subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd, "").
subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd, Quantity) ->
  Fields = [{"plan", Plan},
    {"coupon", Coupon},
    {"prorate", Prorate},
    {"trial_end", TrialEnd},
    {"quantity", Quantity}],
  request_subscription(subscribe, Customer, Fields).

subscription_update(Customer, Subscription, Fields) ->
  request_subscription(update, Customer, Subscription, Fields).

subscription_cancel(Customer, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  request_subscription(unsubscribe, Customer, Fields, AtPeriodEnd).

subscription_cancel(Customer, Subscription, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  request_subscription(unsubscribe, Customer, Subscription, Fields, AtPeriodEnd).

%%%--------------------------------------------------------------------
%%% Recipient Management
%%%--------------------------------------------------------------------
recipient_create(Name, Type, TaxId, Bank, Email, Desc) ->
  Fields = [{name, Name},
    {type, Type},
    {tax_id, TaxId},
    {bank_account, Bank},
    {email, Email},
    {description, Desc}],
  request_recipient_create(Fields).

recipient_update(RecipientId, Name, TaxId, Bank, Email, Desc) ->
  Fields = [{name, Name},
    {tax_id, TaxId},
    {bank_account, Bank},
    {email, Email},
    {description, Desc}],
  request_recipient_update(RecipientId, Fields).

%%%--------------------------------------------------------------------
%%% Transfers (Payout) Management
%%%--------------------------------------------------------------------
transfer_create(Amount, Currency, RecipientId, Desc, StatementDesc) ->
  Fields = [{amount, Amount},
    {currency, Currency},
    {recipient, RecipientId},
    {description, Desc},
    {statement_descriptor, StatementDesc}],
  request_transfer_create(Fields).

transfer_cancel(TransferId) ->
  request_transfer_cancel(TransferId).

%%%--------------------------------------------------------------------
%%% event retrieval
%%%--------------------------------------------------------------------
event(EventId) ->
  request_event(EventId).

customer(CustomerId) ->
  request_customer(CustomerId).

%%%--------------------------------------------------------------------
%%% InvoiceItem Support
%%%--------------------------------------------------------------------

invoiceitem(InvoiceItemId) ->
  request_invoiceitem(InvoiceItemId).

invoiceitem_create(Customer, Amount, Currency, Description) ->
  Fields = [{customer, Customer},
    {amount, Amount},
    {currency, Currency},
    {description, Description}],
  request_invoiceitem_create(Fields).

%%%--------------------------------------------------------------------
%%% Pagination Support
%%%--------------------------------------------------------------------

get_all_customers() ->
  request_all_customers().

get_num_customers(Count) ->
  request_num_customers(Count).

%%%--------------------------------------------------------------------
%%% request generation and sending
%%%--------------------------------------------------------------------
request_charge(Fields) ->
  request(charges, post, Fields).

request_event(EventId) ->
  request_run(gen_event_url(EventId), get, []).

request_customer(CustomerId) ->
  request_run(gen_customer_url(CustomerId), get, []).

request_invoiceitem(InvoiceItemId) ->
  request_run(gen_invoiceitem_url(InvoiceItemId), get, []).

request_invoiceitem_create(Fields) ->
  request(invoiceitems, post, Fields).

request_account_create(Fields) ->
  request(accounts, post, Fields).

request_account_update(AccountId, Fields) ->
  request_run(gen_account_url(AccountId), post, Fields).

request_account_update_subresource(AccountId, Resource, Fields) ->
  request_run(gen_account_subresource_url(AccountId, Resource), post, Fields).

request_account_delete_subresource(AccountId, Resource, ResourceId) ->
  request_run(gen_account_subresource_url(AccountId, Resource) ++
    "/" ++ ResourceId, delete, []).

request_customer_update_subresource(CustomerId, Resource, Fields) ->
  request_run(gen_customer_subresource_url(CustomerId, Resource), post, Fields).

request_customer_delete_subresource(CustomerId, Resource, ResourceId) ->
  request_run(gen_customer_subresource_url(CustomerId, Resource) ++
    "/" ++ ResourceId, delete, []).

request_plans() ->
  request_run(gen_url(plans), get, []).

request_account(AccountId) ->
  request_run(gen_account_url(AccountId), get, []).

request_customer_create(Fields) ->
  request(customers, post, Fields).

request_customer_update(CustomerId, Fields) ->
  request_run(gen_customer_url(CustomerId), post, Fields).

request_token_create(Fields) ->
  request(tokens, post, Fields).

request_recipient_create(Fields) ->
  request(recipients, post, Fields).

request_recipient_update(RecipientId, Fields) ->
  request_run(gen_recipient_url(RecipientId), post, Fields).

request_transfer_create(Fields) ->
  request(transfers, post, Fields).

request_transfer_cancel(TransferId) ->
  request_run(gen_transfer_cancel_url(TransferId), post, []).

request(Action, post, Fields) ->
  URL = gen_url(Action),
  request_run(URL, post, Fields).

request_subscription(subscribe, Customer, Fields) ->
  request_run(gen_subscription_url(Customer), post, Fields).

request_subscription(update, Customer, Subscription, Fields) ->
  request_run(gen_subscription_url(Customer, Subscription), post, Fields);

request_subscription(unsubscribe, Customer, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer), delete, Fields).

request_subscription(unsubscribe, Customer, Subscription, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer, Subscription) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Subscription, Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer, Subscription), delete, Fields).

request_all_customers() ->
  request_all(customers).

request_num_customers(Count) when Count =< ?STRIPE_LIST_LIMIT ->
  request_run(gen_paginated_url(customers, Count), get, []);
request_num_customers(Count) ->
  error_logger:error_msg("Requested ~p customers when ~p is the maximum allowed~n",
    [Count, ?STRIPE_LIST_LIMIT]).

%% Request all items in a pagination supported type
%% This will continue to call ?STRIPE_LIST_LIMIT items
%% until no items are remaining. If attempting to test
%% be sure to set large timeouts for listing huge accounts
request_all(Type) ->
  request_all(Type, []).
request_all(Type, StartingAfter) ->
  case request_run_all(gen_paginated_url(Type,
    ?STRIPE_LIST_LIMIT,
    StartingAfter)) of
    {error, Reason} ->
      {error, Reason};
    {false, Results} ->
      Results#stripe_list.data;
    {true, Results} ->
      TypeList = Results#stripe_list.data,
      LastElement = lists:last(TypeList),
      LastElementId = get_record_id(LastElement),
      TypeList ++ request_all(Type, LastElementId)
  end.

get_record_id(Type) when is_record(Type, stripe_customer) ->
  Type#stripe_customer.id;
get_record_id(Type) when is_record(Type, stripe_account) ->
  Type#stripe_account.id;
get_record_id(Type) when is_record(Type, stripe_charge) ->
  Type#stripe_charge.id;
get_record_id(Type) when is_record(Type, stripe_error) ->
  {stripe_error, ErrorType, _ErrorCode, ErrorMessage, _ErrorParam,
    _HttpErrorCode, _HttpErrorCodeMeaning} = Type,
  {error, ErrorType, ErrorMessage};
get_record_id(_Type) -> error. % anything else

request_run(URL, Method, Fields) ->
  Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
    {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
    {"Authorization", auth_key()}],
  Type = "application/x-www-form-urlencoded",
  Body = gen_args(Fields),
  Request =
    case Method of
    % get and delete are body-less http requests
      get -> {URL, Headers};
      delete -> {URL, Headers};
      _ -> {URL, Headers, Type, Body}
    end,
  Requested = httpc:request(Method, Request, [], []),
  resolve(Requested).

%% Much like request_run/3 except that a tuple is returned with the
%% results indicating more results are available
%% Returns:
%%   {error, Reason} - Same as request_run
%%   {false, Results} - No more results left, returns current page list
%%   {true, Results} - There are more results left, returns current page list
request_run_all(URL) ->
  Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
    {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
    {"Authorization", auth_key()}],
  Request = {URL, Headers},
  Requested = httpc:request(get, Request, [], []),

  case resolve(Requested) of
    {error, _} = Error ->
      Error;
    Results ->
      {has_more(Requested), Results}
  end.

%% Simple function that checks if the body has more results in a paginated query
has_more({ok, {{_HTTPVer, _StatusCode, _Reason}, _Headers, Body}}) ->
  DecodedResult = json:decode(Body),
  proplists:get_value(<<"has_more">>, DecodedResult).

%%%--------------------------------------------------------------------
%%% response parsing
%%%--------------------------------------------------------------------
resolve({ok, {{_HTTPVer, StatusCode, _Reason}, _Headers, Body}}) ->
  resolve_status(StatusCode, Body);
resolve({ok, {StatusCode, Body}}) ->
  resolve_status(StatusCode, Body);
resolve({error, Reason}) ->
  {error, Reason}.

-spec resolve_status(pos_integer(), json()) ->
  #stripe_card{} | #stripe_token{} | #stripe_event{} |
  #stripe_customer{} | #stripe_error{}.
% success range conditions stolen from stripe-python
resolve_status(HTTPStatus, SuccessBody) when
  HTTPStatus >= 200 andalso HTTPStatus < 300 ->
  json_to_record(SuccessBody);
resolve_status(HTTPStatus, ErrorBody) ->
  json_to_error(HTTPStatus, ErrorBody).

%%%--------------------------------------------------------------------
%%% Json to local type object records
%%%--------------------------------------------------------------------
-define(NRAPI, <<"Not Returned by API">>).
-define(V(X), json:get_value(atom_to_binary(X, utf8),
                             DecodedResult, ?NRAPI)).

json_to_record(Json) when is_list(Json) andalso is_tuple(hd(Json)) ->
  json_to_record(proplists:get_value(<<"object">>, Json), Json);

json_to_record(Body) when is_list(Body) orelse is_binary(Body) ->
  DecodedResult = json:decode(Body),
  json_to_record(DecodedResult).

% Yes, these are verbose and dumb because we don't have runtime record/object
% capabilities.  In a way, it's nice being explicit up front.
-spec json_to_record(stripe_object_name(), proplist()) -> #stripe_list{}.
json_to_record(<<"list">>, DecodedResult) ->
  Data = ?V(data),
  #stripe_list{data = [json_to_record(Object) || Object <- Data]};

json_to_record(<<"event">>, DecodedResult) ->
  Data = ?V(data),
  Object = proplists:get_value(<<"object">>, Data),
  ObjectName = proplists:get_value(<<"object">>, Object),
  #stripe_event{id = ?V(id),
    type = ?V(type),
    created = ?V(created),
    data = json_to_record(ObjectName, Object)};

json_to_record(<<"charge">>, DecodedResult) ->
  #stripe_charge{id = ?V(id),
    created = ?V(created),
    amount = ?V(amount),
    balance_transaction = ?V(balance_transaction),
    currency = check_to_atom(?V(currency)),
    description = ?V(description),
    livemode = ?V(livemode),
    paid = ?V(paid),
    refunded = ?V(refunded),
    customer = ?V(customer),
    failure_code = ?V(failure_code),
    failure_message = ?V(failure_message),
    card = proplist_to_card(?V(card))};

json_to_record(<<"token">>, DecodedResult) ->
  #stripe_token{id = ?V(id),
    used = ?V(used),
    livemode = ?V(livemode),
    card = proplist_to_card(?V(card)),
    bank_account = proplist_to_bank_account(?V(bank_account))};

json_to_record(<<"customer">>, DecodedResult) ->
  #stripe_customer{id = ?V(id),
    description = ?V(description),
    livemode = ?V(livemode),
    created = ?V(created),
    email = ?V(email),
    delinquent = ?V(delinquent),
    discount = json_to_record(<<"discount">>, ?V(discount)),
    account_balance = ?V(account_balance),
    sources = proplist_to_customer_sources(?V(sources)),
    subscriptions = (json_to_record(<<"list">>, ?V(subscriptions)))#stripe_list.data};

% We don't have eunit tests for discount decoding yet.  Use at your own risk.
json_to_record(<<"discount">>, null) -> null;
json_to_record(<<"discount">>, DecodedResult) ->
  #stripe_discount{coupon = json_to_record(coupon, ?V(coupon)),
    start = ?V(start),
    'end' = ?V('end'),
    customer = ?V(customer)
  };

% We don't have eunit tests for coupon decoding yet.  Use at your own risk.
json_to_record(<<"coupon">>, null) -> null;
json_to_record(<<"coupon">>, DecodedResult) ->
  #stripe_coupon{id = ?V(id),
    percent_off = ?V(percent_off),
    amount_off = ?V(amount_off),
    currency = check_to_atom(?V(currency)),
    duration = ?V(duration),
    redeem_by = ?V(redeem_by),
    max_redemptions = ?V(max_redemptions),
    times_redeemed = ?V(times_redeemed),
    duration_in_months = ?V(duration_in_months)
  };

json_to_record(<<"subscription">>, null) -> null;
json_to_record(<<"subscription">>, DecodedResult) when is_list(DecodedResult) ->
  #stripe_subscription{id = ?V(id),
    status = check_to_atom(?V(status)),
    current_period_start = ?V(current_period_start),
    current_period_end = ?V(current_period_end),
    trial_start = ?V(trial_start),
    trial_end = ?V(trial_end),
    ended_at = ?V(ended_at),
    canceled_at = ?V(canceled_at),
    customer = ?V(customer),
    start = ?V(start),
    quantity = ?V(quantity),
    plan = proplist_to_plan(?V(plan))};

json_to_record(<<"invoiceitem">>, DecodedResult) ->
  #stripe_invoiceitem{id = ?V(id),
    amount = ?V(amount),
    currency = check_to_atom(?V(currency)),
    customer = ?V(customer),
    date = ?V(date),
    description = ?V(description),
    proration = ?V(proration)};

json_to_record(<<"recipient">>, DecodedResult) ->
  #stripe_recipient{id = ?V(id),
    created = ?V(created),
    type = check_to_atom(?V(type)),
    active_account = proplist_to_bank_account(?V(active_account)),
    verified = ?V(verified),
    description = ?V(description),
    name = ?V(name),
    email = ?V(email)};

json_to_record(<<"transfer">>, DecodedResult) ->
  #stripe_transfer{id = ?V(id),
    amount = ?V(amount),
    currency = check_to_atom(?V(currency)),
    date = ?V(date),
    balance_transaction = ?V(balance_transaction),
    status = check_to_atom(?V(status)),
    account = proplist_to_bank_account(?V(account)),
    description = ?V(description),
    recipient = ?V(recipient),
    statement_descriptor = ?V(statement_descriptor)};

json_to_record(<<"bank_account">>, DecodedResult) ->
  proplist_to_bank_account(DecodedResult);

json_to_record(<<"account">>, DecodedResult) ->
  #stripe_account{id = ?V(id),
    email = ?V(email),
    statement_descriptor = ?V(statement_descriptor),
    display_name = ?V(display_name),
    timezone = ?V(timezone),
    details_submitted = ?V(details_submitted),
    charges_enabled = ?V(charges_enabled),
    transfers_enabled = ?V(transfers_enabled),
    currencies_supported = ?V(currencies_supported),
    default_currency = ?V(default_currency),
    country = ?V(country),
    business_name = ?V(business_name),
    business_url = ?V(business_url),
    support_phone = ?V(support_phone),
    business_logo = ?V(business_logo),
    managed = ?V(managed),
    product_description = ?V(product_description),
    debit_negative_balances = ?V(debit_negative_balances),
    bank_accounts = proplist_to_bank_account_list(?V(bank_accounts)),
    external_accounts = proplist_to_bank_account_list(?V(external_accounts)),
    verification = proplist_to_stripe_verification(?V(verification)),
    transfer_schedule = proplist_to_transfer_schedule(?V(transfer_schedule)),
    decline_charge_on = proplist_to_decline_charge_on(?V(decline_charge_on)),
    tos_acceptance = proplist_to_tos_acceptance(?V(tos_acceptance)),
    legal_entity = proplist_to_legal_entity(?V(legal_entity))};

json_to_record(<<"card">>, DecodedResult) ->
  #stripe_card{id = ?V(id),
    name = ?V(name),
    last4 = ?V(last4),
    funding = ?V(funding),
    exp_month = ?V(exp_month),
    exp_year = ?V(exp_year),
    brand = ?V(brand),
    address_line1 = ?V(address_line1),
    address_line2 = ?V(address_line2),
    address_city = ?V(address_city),
    address_zip = ?V(address_zip),
    address_state = ?V(address_state),
    address_country = ?V(address_country),
    cvc_check = check_to_atom(?V(cvc_check)),
    address_line1_check = check_to_atom(?V(address_line1_check)),
    address_zip_check = check_to_atom(?V(address_zip_check)),
    tokenization = ?V(tokenization),
    dynamic_last4 = ?V(dynamic_last4),
    country = ?V(country),
    fingerprint = ?V(fingerprint),
    recipient = ?V(recipient),
    customer = ?V(customer),
    account = ?V(account),
    currency = ?V(currency),
    default_for_currency = ?V(default_for_currency)};

json_to_record(<<"plan">>, DecodedResult) ->
  proplist_to_plan(DecodedResult);

json_to_record(undefined, DecodedResult) ->
  % let's check here if maybe this is a reply for a DELETE operation
  case ?V(deleted) of
    true -> {deleted, ?V(id)};
    _ -> json_to_record(unknown_type, DecodedResult)
  end;

json_to_record(Type, DecodedResult) ->
  error_logger:error_msg("Stripe : ~p", [[{unimplemented, ?MODULE, json_to_record, Type, DecodedResult}]]),
  {not_implemented_yet, Type, DecodedResult}.

proplist_to_transfer_schedule(null) -> null;
proplist_to_transfer_schedule(A) when is_binary(A) -> A;
proplist_to_transfer_schedule(DecodedResult) ->
  #stripe_transfer_schedule{interval = check_to_atom(?V(interval)),
    monthly_anchor = ?V(monthly_anchor),
    weekly_anchor = ?V(weekly_anchor)}.

proplist_to_decline_charge_on(null) -> null;
proplist_to_decline_charge_on(A) when is_binary(A) -> A;
proplist_to_decline_charge_on(DecodedResult) ->
  #stripe_decline_charge_on{cvc_failure = check_to_atom(?V(cvc_failure)),
    avs_failure = check_to_atom(?V(avs_failure))}.

proplist_to_tos_acceptance(null) -> null;
proplist_to_tos_acceptance(A) when is_binary(A) -> A;
proplist_to_tos_acceptance(DecodedResult) ->
  #stripe_tos_acceptance{ip = ?V(ip),
    date = ?V(date),
    user_agent = ?V(user_agent)}.


proplist_to_stripe_verification(null) -> null;
proplist_to_stripe_verification(A) when is_binary(A) -> A;
proplist_to_stripe_verification(DecodedResult) ->
  #stripe_verification{fields_needed = ?V(fields_needed),
    disabled_reason = ?V(disabled_reason),
    due_by = ?V(due_by)}.

proplist_to_legal_entity(null) -> null;
proplist_to_legal_entity(A) when is_binary(A) -> A;
proplist_to_legal_entity(DecodedResult) ->
  #stripe_legal_entity{business_name = ?V(business_name),
    address = proplist_to_address(?V(address)),
    first_name = ?V(first_name),
    last_name = ?V(last_name),
    personal_address = proplist_to_address(?V(personal_address)),
    dob = proplist_to_date(?V(dob)),
    verification = proplist_to_stripe_legal_entity_verification(?V(verification))}.

proplist_to_address(null) -> null;
proplist_to_address(A) when is_binary(A) -> A;
proplist_to_address(DecodedResult) ->
  #address{line1 = ?V(line1),
    line2 = ?V(line2),
    city = ?V(city),
    state = ?V(state),
    postal_code = ?V(postal_code),
    country = ?V(country)}.

proplist_to_date(null) -> null;
proplist_to_date(A) when is_binary(A) -> A;
proplist_to_date(DecodedResult) ->
  #date{day = ?V(day),
    month = ?V(month),
    year = ?V(year)}.

proplist_to_stripe_legal_entity_verification(null) -> null;
proplist_to_stripe_legal_entity_verification(A) when is_binary(A) -> A;
proplist_to_stripe_legal_entity_verification(DecodedResult) ->
  #stripe_legal_entity_verification{status = check_to_atom(?V(status)),
    details = ?V(details),
    document = ?V(document)}.

proplist_to_card(null) -> null;
proplist_to_card(A) when is_binary(A) -> A;
proplist_to_card(Card) ->
  DecodedResult = Card,
  #stripe_card{id = ?V(id),
    name = ?V(name),
    last4 = ?V(last4),
    funding = ?V(funding),
    exp_month = ?V(exp_month),
    exp_year = ?V(exp_year),
    brand = ?V(brand),
    address_line1 = ?V(address_line1),
    address_line2 = ?V(address_line2),
    address_city = ?V(address_city),
    address_state = ?V(address_state),
    address_zip = ?V(address_zip),
    address_country = ?V(address_country),
    cvc_check = check_to_atom(?V(cvc_check)),
    address_line1_check = check_to_atom(?V(address_line1_check)),
    address_zip_check = check_to_atom(?V(address_zip_check)),
    tokenization = ?V(tokenization),
    dynamic_last4 = ?V(dynamic_last4),
    country = ?V(country),
    fingerprint = ?V(fingerprint),
    recipient = ?V(recipient),
    customer = ?V(customer),
    account = ?V(account),
    currency = ?V(currency),
    default_for_currency = ?V(default_for_currency)}.

proplist_to_plan(Plan) ->
  DecodedResult = Plan,
  #stripe_plan{id = ?V(id),
    amount = ?V(amount),
    created = ?V(created),
    currency = check_to_atom(?V(currency)),
    interval = ?V(interval),
    interval_count = ?V(interval_count),
    livemode = ?V(livemode),
    metadata = ?V(metadata),
    name = ?V(name),
    statement_descriptor = ?V(statement_descriptor),
    trial_period_days = ?V(trial_period_days)
  }.

proplist_to_bank_account(null) -> null;
proplist_to_bank_account(A) when is_binary(A) -> A;
proplist_to_bank_account(BankAccount) ->
  DecodedResult = BankAccount,
  #stripe_bank_account{id = ?V(id),
    currency = ?V(currency),
    default_for_currency = check_to_atom(?V(default_for_currency)),
    % metadata = ???
    fingerprint = ?V(fingerprint),
    bank_name = ?V(bank_name),
    last4 = ?V(last4),
    country = ?V(country),
    status = check_to_atom(?V(status)),
    routing_number = ?V(routing_number),
    account = ?V(account)}.

proplist_to_customer_sources(null) -> null;
proplist_to_customer_sources(A) when is_binary(A) -> A;
proplist_to_customer_sources(DecodedResult) ->
  SrcList = ?V(data),
  Fun = fun(Src) -> proplist_to_bank_or_card(Src) end,
  lists:map(Fun, SrcList).

proplist_to_bank_or_card(DecodedResult) ->
  io:format("proplist_to_bank_or_card() : input is ~p", [DecodedResult]),
  Object = ?V(object),
  if
    Object == <<"bank_account">> -> proplist_to_bank_account(DecodedResult);
    Object == <<"card">> -> proplist_to_card(DecodedResult);
    true -> io:format("proplist_to_bank_or_card() cannot handle object type ~p", [Object]),
      []
  end.

proplist_to_bank_account_list(null) -> null;
proplist_to_bank_account_list(A) when is_binary(A) -> A;
proplist_to_bank_account_list(BankAccountsDictionary) ->
  DecodedResult = BankAccountsDictionary,
  BAList = ?V(data),
  Fun = fun(BA) -> proplist_to_bank_account(BA) end,
  lists:map(Fun, BAList).

check_to_atom(null) -> null;
check_to_atom(A) when is_atom(A) -> A;
check_to_atom(Check) when is_binary(Check) -> binary_to_atom(Check, utf8).

% error range conditions stolen from stripe-python
json_to_error(ErrCode, Body) ->
  ErrCodeMeaning = case ErrCode of
                     400 -> missing_param;
                     401 -> bad_api_key;
                     402 -> params_ok_but_request_failed;
                     404 -> notfound;
                     E when E >= 500 -> stripe_server_error;
                     E when E =:= 403 orelse E > 404 -> stripe_api_error;
                     _ -> unknown_error
                   end,
  json_to_error(ErrCode, ErrCodeMeaning, Body).

% Let's use a common error object/record instead of breaking out per-type
% errors.  We can match on error types easily.
json_to_error(ErrCode, ErrCodeMeaning, Body) ->
  PreDecoded = json:decode(Body),
  DecodedResult = json:get_value(<<"error">>, PreDecoded),
  #stripe_error{type    = check_to_atom(?V(type)),
                code    = check_to_atom(?V(code)),
                http_error_code = ErrCode,
                http_error_code_meaning = ErrCodeMeaning,
                message = ?V(message),
                param   = ?V(param)}.

%%%--------------------------------------------------------------------
%%% value helpers
%%%--------------------------------------------------------------------
ua_json() ->
  Props = [{<<"bindings_version">>, ?VSN_BIN},
           {<<"lang">>, <<"erlang">>},
           {<<"publisher">>, <<"mattsta">>}],
  binary_to_list(iolist_to_binary(json:encode(Props))).

auth_key() ->
  Token = env(auth_token),
  Auth = base64:encode_to_string(Token ++ ":"),
  "Basic " ++ Auth.

env(What) ->
  case env(What, diediedie) of
    diediedie -> throw({<<"You must define this in your app:">>, What});
    Else -> Else
  end.

env(What, Default) ->
  case application:get_env(stripe, What) of
    {ok, Found} -> Found;
    undefined -> Default
  end.

-spec gen_args(proplist()) -> string().
gen_args([]) -> "";
gen_args(Fields) when is_list(Fields) andalso is_tuple(hd(Fields)) ->
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  mochiweb_util:urlencode(OnlyWithValues).

gen_url(Action) when is_atom(Action) ->
  gen_url(atom_to_list(Action));
gen_url(Action) when is_list(Action) ->
  "https://api.stripe.com/v1/" ++ Action.

gen_account_url(AccountId) when is_binary(AccountId) ->
  gen_account_url(binary_to_list(AccountId));
gen_account_url(AccountId) when is_list(AccountId) ->
  "https://api.stripe.com/v1/accounts/" ++ AccountId.

gen_account_subresource_url(AccountId, Resource) when is_binary(AccountId) orelse
    is_binary(Resource) ->
  AccountIdList = if
                    is_binary(AccountId) -> binary_to_list(AccountId);
                    true -> AccountId
                  end,
  ResourceList = if
                   is_binary(Resource) -> binary_to_list(Resource);
                   true -> Resource
                 end,
  gen_account_subresource_url(AccountIdList, ResourceList);
gen_account_subresource_url(AccountId, Resource) when is_list(AccountId) and
    is_list(Resource) ->
  gen_account_url(AccountId) ++ "/" ++ Resource.

gen_customer_subresource_url(CustomerId, Resource) when is_binary(CustomerId) orelse
    is_binary(Resource) ->
  CustomerIdList =
    if
      is_binary(CustomerId) -> binary_to_list(CustomerId);
      true -> CustomerId end,
  ResourceList =
    if
      is_binary(Resource) -> binary_to_list(Resource);
      true -> Resource end,
  gen_customer_subresource_url(CustomerIdList, ResourceList);
gen_customer_subresource_url(CustomerId, Resource) when is_list(CustomerId) and
    is_list(Resource) ->
  gen_customer_url(CustomerId) ++ "/" ++ Resource.

gen_charge_subresource_url(ChargeId, SubResource) ->
  gen_url(chanrges) ++ "/" ++ ChargeId ++ atom_to_list(SubResource).

gen_customer_url(CustomerId) when is_binary(CustomerId) ->
  gen_customer_url(binary_to_list(CustomerId));
gen_customer_url(CustomerId) when is_list(CustomerId) ->
  "https://api.stripe.com/v1/customers/" ++ CustomerId.

gen_recipient_url(RecipientId) when is_binary(RecipientId) ->
  gen_recipient_url(binary_to_list(RecipientId));
gen_recipient_url(RecipientId) when is_list(RecipientId) ->
  "https://api.stripe.com/v1/recipients/" ++ RecipientId.

gen_transfer_cancel_url(TransferId) when is_binary(TransferId) ->
  gen_transfer_cancel_url(binary_to_list(TransferId));
gen_transfer_cancel_url(TransferId) when is_list(TransferId) ->
  "https://api.stripe.com/v1/transfers/" ++ TransferId ++ "/cancel".

gen_invoiceitem_url(InvoiceItemId) when is_binary(InvoiceItemId) ->
  gen_invoiceitem_url(binary_to_list(InvoiceItemId));
gen_invoiceitem_url(InvoiceItemId) when is_list(InvoiceItemId) ->
  "https://api.stripe.com/v1/invoiceitems/" ++ InvoiceItemId.

gen_subscription_url(Customer) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer));
gen_subscription_url(Customer) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscription".

gen_subscription_url(Customer, Subscription) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer), Subscription);
gen_subscription_url(Customer, Subscription) when is_binary(Subscription) ->
  gen_subscription_url(Customer, binary_to_list(Subscription));
gen_subscription_url(Customer, Subscription) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscriptions/" ++ Subscription.

gen_event_url(EventId) when is_binary(EventId) ->
  gen_event_url(binary_to_list(EventId));
gen_event_url(EventId) when is_list(EventId) ->
  "https://api.stripe.com/v1/events/" ++ EventId.


gen_paginated_url(Type) ->
  gen_paginated_url(Type, 10, [], []).
gen_paginated_url(Type, Limit) ->
  gen_paginated_url(Type, Limit, [], []).
gen_paginated_url(Type, Limit, StartingAfter) ->
  gen_paginated_url(Type, Limit, StartingAfter, []).
gen_paginated_url(Type, Limit, StartingAfter, EndingBefore) ->
  Arguments = gen_args([{"limit", Limit},
    {"starting_after", StartingAfter},
    {"ending_before", EndingBefore}]),
  gen_paginated_base_url(Type) ++ Arguments.

gen_paginated_base_url(charges) ->
  "https://api.stripe.com/v1/charges?";
gen_paginated_base_url(customers) ->
  "https://api.stripe.com/v1/customers?";
gen_paginated_base_url(invoices) ->
  "https://api.stripe.com/v1/invoices?".
