import { Watch } from 'react-loader-spinner';


export default function WatchLoader({ id, color }) {
  console.log(Watch);
  return (
    <div id={id}>
      <Watch
        visible={true}
        height="80"
        width="80"
        radius="48"
        color={color}
        ariaLabel="watch-loading"
        wrapperStyle={{}}
        wrapperClass=""
      />
    </div>
  );
}
