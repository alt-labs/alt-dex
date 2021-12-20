import { FC, useState } from 'react';
import useOnClickOutside from '../../../hooks/useOnClickOutside';
import { append, map } from '../../../util/ts-belt/Array';
import { not } from '../../../util/ts-belt/Boolean';
import { prop } from '../../../util/ts-belt/Dict';
import { mapWithDefault } from '../../../util/ts-belt/Option';
import { AVAILABLE_TOKENS } from '../../constants';
import { MaybeTokenWithAmount, Token } from '../../types';
import {
  Input,
  SelectedTokenImage,
  SelectTokenButton,
  SelectTokenDropdown,
  SwapBlockBody,
  TokenDropdownItem,
  TokenImage,
  TokenName,
  TokenSymbol,
} from './styled';

type Props = {
  tokenWithAmount: MaybeTokenWithAmount;
  onTokenSelect: (token: Token) => void;
  disabledTokens: Token[];
};

const SwapInput: FC<Props> = ({
  tokenWithAmount,
  onTokenSelect,
  disabledTokens,
}) => {
  const [isDropdownOpen, setIsDropdownOpen] = useState(false);

  const dropdownRef = useOnClickOutside<HTMLUListElement>(() =>
    setIsDropdownOpen(false),
  );

  const toggleDropdown = () => {
    setIsDropdownOpen(not);
  };

  const handleTokenClick = (token: Token) => () => {
    onTokenSelect(token);
    setIsDropdownOpen(false);
  };

  const { tokenOption, amount } = tokenWithAmount;
  const otherDisabledTokens = map(disabledTokens, prop('symbol'));
  const disabledTokenSymbols = mapWithDefault(
    tokenOption,
    otherDisabledTokens,
    (token) => append(otherDisabledTokens, token.symbol),
  );

  return (
    <SwapBlockBody>
      <Input
        type="number"
        value={amount}
        placeholder="0.0"
        autoComplete="off"
        autoCorrect="off"
        step="0.01"
        disabled
      />
      <SelectTokenButton isSelected={isDropdownOpen} onClick={toggleDropdown}>
        {mapWithDefault(
          tokenOption,
          <span>Select a token</span>,
          ({ img, name, symbol }) => (
            <span>
              <SelectedTokenImage src={img} alt={name} width={24} height={24} />
              {symbol}
            </span>
          ),
        )}
      </SelectTokenButton>
      <SelectTokenDropdown isActive={isDropdownOpen} ref={dropdownRef}>
        {AVAILABLE_TOKENS.map((token) => (
          <TokenDropdownItem
            key={token.symbol}
            isDisabled={disabledTokenSymbols.includes(token.symbol)}
            onClick={handleTokenClick(token)}
          >
            <TokenImage
              src={token.img}
              alt={token.name}
              width={24}
              height={24}
            />
            <div>
              <TokenSymbol>{token.symbol}</TokenSymbol>
              <TokenName>{token.name}</TokenName>
            </div>
          </TokenDropdownItem>
        ))}
      </SelectTokenDropdown>
    </SwapBlockBody>
  );
};

export default SwapInput;
